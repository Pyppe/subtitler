package fi.pyppe.subtitler

import java.io._
import java.util.zip.GZIPInputStream

import com.ning.http.util.Base64
import org.joda.time.DateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.{Elem, XML}

object MatchedBy extends Enumeration {
  type MatchedBy = Value
  val MovieHash, Imdb, Tag, FullText = Value
}
import MatchedBy._

case class Subtitle(matchedBy: MatchedBy, idSubMovieFile: String, hash: String, movieByteSize: Long,
                    idSubtitleFile: String, subFileName: String, idSubtitle: String, languageCode: String,
                    format: String, cdCount: Int, downloadCount: Int, rating: Double, badCount: Int, idMovie: String,
                    imdbId: String, movieName: String, movieNameEng: String, movieYear: Int) {
  def downloadId: String = idSubtitleFile
  def formatSafe: String = Option(format.replace(".", "").toLowerCase).filter(_.nonEmpty).getOrElse("sub")
}
object Subtitle {
  def empty: Subtitle = Subtitle(MatchedBy.Tag, "", "", 0L, "", "", "", "eng", "srt", 1, 0, 0.0d, 0, "", "", "", "", 2014)
}
case class SubtitleData(id: String, encodedData: String) {
  lazy val content: String = new String(OpenSubtitlesDecoder.decodeAndDecompress(encodedData), "utf-8")
  override def toString() = s"SubtitleData($id, encodedLength = ${encodedData.length})"
}

object OpenSubtitlesAPI extends Logging {
  import HttpUtils._
  import OpenSubtitlesHasher.computeHash

  @volatile
  private var expiringToken: Option[(String, DateTime)] = None
  private val EndPoint = "http://api.opensubtitles.org/xml-rpc"
  //private val EndPoint = "http://localhost:8080/xml-rpc"


  def serverInfo(): Future[Elem] = {
    postXML(EndPoint, <methodCall><methodName>ServerInfo</methodName></methodCall>)
  }

  private def parseRootMembers(xml: Elem): List[(String, String)] = {
    (xml \\ "member").map { member =>
      (member \ "name").text.trim -> (member \ "value").text.trim
    }.toList
  }

  def parseSearchResponse(xml: Elem): List[Subtitle] = {

    def matchedBy(value: String) = value match {
      case "moviehash" => MovieHash
      case "imdbid"    => Imdb
      case "tag"       => Tag
      case "fulltext"  => FullText
    }

    def subtitle(fields: Map[String, String]) = Subtitle(
      matchedBy(fields("MatchedBy")),
      fields("IDSubMovieFile"),
      fields("MovieHash"),
      fields("MovieByteSize").toLong,
      fields("IDSubtitleFile"),
      fields("SubFileName"),
      fields("IDSubtitle"),
      fields("ISO639"),
      fields("SubFormat"),
      fields("SubSumCD").toInt,
      fields("SubDownloadsCnt").toInt,
      fields("SubRating").toDouble,
      fields("SubBad").toInt,
      fields("IDMovie"),
      fields("IDMovieImdb"),
      fields("MovieName"),
      fields("MovieNameEng"),
      fields("MovieYear").toInt
    )

    val dataMembers = (for {
      member <- xml \\ "member" if (member \ "name").text == "data"
      dataMember <- member \\ "struct" \\ "member"
    } yield {
      (dataMember \ "name").text.trim -> (dataMember \ "value").text.trim
    }).toList

    val (acc, fields) = dataMembers.foldLeft((List.empty[Subtitle], Map.empty[String, String])) {
      case ((acc, fields), (key, value)) =>
        key match {
          case "MatchedBy" if fields.nonEmpty =>
            (subtitle(fields) :: acc) -> Map(key -> value)
          case _ =>
            acc -> (fields + (key -> value))
        }
    }

    val results =
      if (fields.nonEmpty)
        (subtitle(fields) :: acc).reverse
      else
      acc.reverse

    results.groupBy(_.downloadId).values.map(_.head).toList
  }


  private def withValidToken[T](action: String => Future[T])
                               (implicit settings: Settings): Future[T] = {
    val future = expiringToken.filter( _._2.plusMinutes(14).isAfterNow ).map {
      case (token, _) =>
        action(token)
    }.getOrElse {
      logIn flatMap action
    }
    future.onSuccess {
      case _ =>
        expiringToken = expiringToken.map {
          case (token, _) => (token, DateTime.now)
        }
    }

    future
  }

  def logIn()(implicit settings: Settings): Future[String] = {
    val os = settings.openSubtitlesConf
    val language = "en"
    val req =
      <methodCall>
        <methodName>LogIn</methodName>
        <params>
          <param><value><string>{os.login}</string></value></param>
          <param><value><string>{os.password}</string></value></param>
          <param><value><string>{language}</string></value></param>
          <param><value><string>{os.useragent}</string></value></param>
        </params>
      </methodCall>
    postXML(EndPoint, req).map { xml =>
      val members = parseRootMembers(xml).toMap
      val status = members("status")
      require(status == "200 OK", s"Invalid status: $status")
      val responseToken = members("token")
      expiringToken = Some((responseToken, DateTime.now))
      logger.info(s"Successfully logged-in with token $responseToken")
      responseToken
    }
  }

  def searchSubtitles(f: File)(implicit s: Settings): Future[List[(Subtitle, Double)]] = withValidToken { _ =>
    logger.debug(s"Finding subtitles for ${f.getName}")

    searchSubtitlesByTag(f.getName) zip searchSubtitlesByFileHash(f) map {
      case (byTag, byHash) =>
        val tagIds = byTag.map(_.downloadId).toSet
        val subtitles = byTag ++ byHash.filter(s => tagIds(s.downloadId))
        SubtitleScorer.scoreAndSortCandidates(f.getName, subtitles)
    }
  }

  def searchSubtitle(f: File)(implicit s: Settings): Future[Option[Subtitle]] = {
    searchSubtitles(f).map { candidates =>
      val count = candidates.length
      candidates.headOption.map(_._1).map { best =>
        logger.debug(s"Found subtitle ${best.subFileName} for ${f.getName} out of $count candidates")
        best
      }.orElse {
        logger.debug(s"Could not find subtitle for ${f.getName} (out of $count candidates)")
        None
      }
    }
  }

  def downloadSubtitles(subtitles: Subtitle*)
                       (implicit s: Settings): Future[List[(Subtitle, SubtitleData)]] = withValidToken { token =>
    logger.debug(s"Downloading subtitles: ${subtitles.map(_.subFileName).mkString(" ")}")
    downloadSubtitleIds(subtitles.map(_.downloadId): _*).map { datas =>
      for {
        data <- datas
        subtitle <- subtitles.find(_.downloadId == data.id)
      } yield {
        subtitle -> data
      }
    }
  }

  def downloadSubtitleIds(ids: String*)(implicit s: Settings): Future[List[SubtitleData]] = withValidToken { token =>
    val idValues = ids.map { id =>
      <value><string>{id}</string></value>
    }
    val req =
      <methodCall>
        <methodName>DownloadSubtitles</methodName>
        <params>
          <param><value><string>{token}</string></value></param>
          <param>
            <value>
              <array>
                <data>
                  {idValues}
                </data>
              </array>
            </value>
          </param>
        </params>
      </methodCall>
    postXML(EndPoint, req).map { xml =>
      val values = (xml \\ "array" \\ "member").map { member =>
        (member \ "value").text.trim
      }
      values.grouped(2).collect {
        case Seq(a, b) => SubtitleData(a, b)
      }.toList
    }
  }

  def searchSubtitlesByTag(tag: String)(implicit s: Settings) =
    search("tag" -> tag)

  def searchSubtitlesByFileHash(file: File)(implicit s: Settings) = {
    val hash = computeHash(file)
    val size = file.length
    search(
      "moviehash"     -> hash,
      "moviebytesize" -> size
    )
  }

  private def search(values: (String, Any)*)
                    (implicit s: Settings): Future[List[Subtitle]] = withValidToken { token =>
    val valuesWithLanguage = values :+ ("sublanguageid", s.languages.map(_.id).mkString(","))
    postXML(EndPoint, searchSubtitlesQuery(token, valuesWithLanguage: _*)).
      map(parseSearchResponse)
  }


  private def searchSubtitlesQuery(token: String, values: (String, Any)*): Elem = {
    val members = values.map {
      case (key, value) =>
        val typedValue = value match {
          case i: Int    => <int>{i}</int>
          case l: Long   => <int>{l}</int>
          case s: String => <string>{s}</string>
        }
        <member>
          <name>{key}</name>
          <value>{typedValue}</value>
        </member>
    }.toList
    <methodCall>
      <methodName>SearchSubtitles</methodName>
      <params>
        <param><value><string>{token}</string></value></param>
        <param>
          <value>
            <array>
              <data>
                <value><struct>{members}</struct></value>
              </data>
            </array>
          </value>
        </param>
        <param>
          <value>
            <struct>
              <member>
                <name>limit</name>
                <value><int>500</int></value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
  }

}

object OpenSubtitlesDecoder {
  def decodeAndDecompress(encodedData: String): Array[Byte] = {
    val decodedBytes = Base64.decode(encodedData)

    val gis = new GZIPInputStream(new ByteArrayInputStream(decodedBytes))
    val buf = new Array[Byte](1024)
    val baos = new ByteArrayOutputStream()
    val out = new BufferedOutputStream(baos)
    try {
      var n = gis.read(buf)
      while (n >= 0) {
        out.write(buf, 0, n)
        n = gis.read(buf)
      }
    } finally {
      out.flush
      out.close
    }
    baos.toByteArray
  }
}

// See http://trac.opensubtitles.org/projects/opensubtitles/wiki/HashSourceCodes#Scala
object OpenSubtitlesHasher {
  import java.nio.{LongBuffer, ByteOrder, ByteBuffer}
  import java.nio.channels.FileChannel.MapMode
  import scala.math._

  private val hashChunkSize = 64L * 1024L

  def computeHash(file: File) : String = {
    val fileSize = file.length
    val chunkSizeForFile = min(fileSize, hashChunkSize)

    val fileChannel = new FileInputStream(file).getChannel

    try {
      val head = computeHashForChunk(fileChannel.map(MapMode.READ_ONLY, 0, chunkSizeForFile))
      val tail = computeHashForChunk(fileChannel.map(MapMode.READ_ONLY, max(fileSize - hashChunkSize, 0), chunkSizeForFile))

      "%016x".format(fileSize + head + tail)
    } finally {
      fileChannel.close()
    }
  }

  private def computeHashForChunk(buffer: ByteBuffer) : Long = {
    def doCompute(longBuffer: LongBuffer, hash: Long) : Long = {
      if (!longBuffer.hasRemaining)
        hash
      else
        doCompute(longBuffer, hash + longBuffer.get)
    }
    val longBuffer = buffer.order(ByteOrder.LITTLE_ENDIAN).asLongBuffer()
    doCompute(longBuffer, 0L)
  }

}
