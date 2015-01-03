package fi.pyppe.subtitler

import com.typesafe.config.ConfigFactory
import java.io.File

import org.apache.commons.io.FilenameUtils

import scala.annotation.tailrec

object Main extends App {
  import WSConfig._
  import FileUtils._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Await

  implicit val settings: Settings = {
    import net.ceedubs.ficus.Ficus._
    import net.ceedubs.ficus.readers.ArbitraryTypeReader._
    val config = ConfigFactory.load()
    val watchDirs = config.as[List[String]]("watchDirs") map (new File(_))
    val openSubtitles = config.as[OpenSubtitlesConf]("credentials.openSubtitles")
    Settings(watchDirs, openSubtitles)
  }

  val videoFiles = settings.watchDirs.map(FileUtils.findFiles(_, filter = isVideoFile)).flatten.distinct
  println(videoFiles)



  /*
  val foo = Await.result(WSClient.url("http://www.pyppe.fi/").get.map(_.body), 10.seconds)
  println(foo)
  */


  //WSClient.close()



}

object FileUtils {

  private val VideoExtensions = Set("avi", "mkv", "mp4")
  def isVideoFile(f: File) = f.isFile && VideoExtensions(FilenameUtils.getExtension(f.getName))

  def findFiles(dir: File,
                recursive: Boolean = true,
                filter: File => Boolean = Function.const(true)): List[File] = {
    require(dir.isDirectory, s"Invalid directory: $dir")

    @tailrec
    def findImpl(filesToSearch: List[File], accMarks: List[File]): List[File] = {
      filesToSearch match {
        case Nil =>
          accMarks

        case head :: tail =>
          head match {
            case dir if dir.isDirectory =>
              val nextFiles = (if(recursive) dir.listFiles.toList else Nil) ::: tail
              findImpl(nextFiles, accMarks)

            case file if filter(file) =>
              findImpl(tail, file :: accMarks)

            case _ =>
              findImpl(tail, accMarks)
          }
      }
    }

    findImpl(dir.listFiles.toList, Nil).reverse
  }
}
