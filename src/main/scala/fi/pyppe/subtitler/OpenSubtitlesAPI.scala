package fi.pyppe.subtitler

import java.io.{File, FileInputStream}

import com.typesafe.scalalogging.StrictLogging
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Elem

object MatchedBy extends Enumeration {
  type MatchedBy = Value
  val MovieHash, Imdb, Tag, FullText = Value
}
import MatchedBy._

case class Subtitle(matchedBy: MatchedBy, idSubMovieFile: String, hash: String, movieByteSize: Long,
                    idSubtitleFile: String, subFileName: String, idSubtitle: String, language: String,
                    format: String, cdCount: Int, downloadCount: Int, rating: Double, badCount: Int, idMovie: String,
                    imdbId: String, movieName: String, movieNameEng: String, movieYear: Int)

object OpenSubtitlesAPI extends StrictLogging {
  import WSConfig._
  import OpenSubtitlesHasher.computeHash

  @volatile
  private var token: Option[String] = None
  private val EndPoint = "http://api.opensubtitles.org/xml-rpc"
  //private val EndPoint = "http://localhost:8080/xml-rpc"

  def serverInfo(): Future[Elem] = {
    WSClient.url(EndPoint).post(
      <methodCall><methodName>ServerInfo</methodName></methodCall>
    ).map(_.asXML)
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

    def subtitle(fields: Map[String, String]) = {
      Subtitle(
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
    }

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

    if (fields.nonEmpty) {
      (subtitle(fields) :: acc).reverse
    } else {
      acc.reverse
    }
  }

  def isLoggedIn() = token.isDefined

  def logIn()(implicit settings: Settings) = {
    val os = settings.openSubtitlesConf
    val language = "en"
    WSClient.url(EndPoint).post(
      <methodCall>
        <methodName>LogIn</methodName>
        <params>
          <param><value><string>{os.login}</string></value></param>
          <param><value><string>{os.password}</string></value></param>
          <param><value><string>{language}</string></value></param>
          <param><value><string>{os.useragent}</string></value></param>
        </params>
      </methodCall>
    ).map(_.asXML).map { xml =>
      val members = parseRootMembers(xml).toMap
      val status = members("status")
      require(status == "200 OK", s"Invalid status: $status")
      val responseToken = members("token")
      token = Some(responseToken)
      responseToken
    }
  }

  def searchSubtitles(f: File) = {
    require(isLoggedIn, s"Not logged-in")
    val hash = computeHash(f)
    val size = f.length
    logger.debug(s"Finding subtitles for ${f.getName} [hash = $hash, size = $size]")
    WSClient.url(EndPoint).post(
      <methodCall>
        <methodName>SearchSubtitles</methodName>
        <params>
          <param><value><string>{token.get}</string></value></param>
          <param>
            <value>
              <array>
                <data>
                  <value>
                    <struct>
                      <member>
                        <name>sublanguageid</name>
                        <value><string>eng</string></value>
                      </member>
                      <member>
                        <name>moviehash</name>
                        <value><string>{hash}</string></value>
                      </member>
                      <member>
                        <name>moviebytesize</name>
                        <value><int>{size}</int></value>
                      </member>
                    </struct>
                  </value>
                </data>
              </array>
            </value>
          </param>
          <param>
            <value>
              <struct>
                <member>
                  <name>limit</name>
                  <value><int>1</int></value>
                </member>
              </struct>
            </value>
          </param>
        </params>
      </methodCall>
    ).map { response =>
      val xml = response.asXML

      // TODO: Remove FW
      val fw = new java.io.FileWriter(new File("/tmp/subtitles.xml"))
      fw.write(xml.toString)
      fw.close

      xml
    }.map(parseSearchResponse)
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
      longBuffer.hasRemaining match {
        case false => hash
        case true => doCompute(longBuffer, hash + longBuffer.get)
      }
    }
    val longBuffer = buffer.order(ByteOrder.LITTLE_ENDIAN).asLongBuffer()
    doCompute(longBuffer, 0L)
  }

}