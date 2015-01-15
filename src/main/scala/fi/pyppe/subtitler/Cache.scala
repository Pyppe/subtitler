package fi.pyppe.subtitler

import java.io.{FileWriter, File}
import java.nio.channels.FileChannel
import java.nio.file.{Path, FileSystems}
import org.joda.time.DateTime
import scala.util.{Failure, Success, Try}
import play.api.libs.json.Json

case class IgnoredVideo(file: String, until: DateTime)
case class CacheState(ignoredVideos: List[IgnoredVideo])

object Cache extends Logging {
  implicit val videoFormats = Json.format[IgnoredVideo]
  implicit val csFormats = Json.format[CacheState]
  implicit val codec = io.Codec("utf-8")

  val CacheFile: Option[File] = Try {
    java.util.UUID.randomUUID.toString
    val dir = new File(System.getProperty("java.io.tmpdir"), "subtitler-ac25532d-15bb-45b7-9716-efab95de60b9")
    dir.mkdir()
    val file = new File(dir, "cache.json")
    if (!file.isFile) {
      require(file.createNewFile(), s"Could not create $file")
    }
    file
  } match {
    case Success(f) =>
      Some(f)
    case Failure(err) =>
      logger.warn(s"Cannot initialize cache: ${err.getMessage}")
      None
  }

  def read(): Option[CacheState] = {
    CacheFile.map { f =>
      Try {
        val js = Json.parse(using(io.Source.fromFile(f)) { src =>
          src.getLines.mkString("\n")
        })
        js.as[CacheState]
      } getOrElse {
        logger.info("Initialized empty cache")
        CacheState(Nil)
      }
    }
  }

  def write(cs: CacheState) = CacheFile.foreach { f =>
    try {
      using(new FileWriter(f)) { fw =>
        val js = Json.toJson(cs)
        fw.write(Json.prettyPrint(js))
      }
      logger.debug(s"Wrote cache")
    } catch {
      case err: Throwable =>
        logger.error(s"Could not write cache to $f: ${err.getMessage}")
    }
  }

  private def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A = {
    import scala.language.reflectiveCalls
    try { f(closeable) } finally { closeable.close() }
  }





}
