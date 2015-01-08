package fi.pyppe.subtitler

import com.typesafe.config.ConfigFactory
import java.io.{FileWriter, File}
import org.apache.commons.io.{IOUtils, FilenameUtils}
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Main {
  //import WSConfig._
  import FileUtils._
  import scala.concurrent.duration._
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global

  case class Params(files: Seq[File] = Nil, interactive: Boolean = false)
  object Params {
    val ImdbId = """.*(tt[0-9]+).*""".r
  }


  val parser = new scopt.OptionParser[Params]("subtitler") {
    head("subtitler", "1.0")

    /*
    // --file <video-file>
    opt[File]('f', "file") optional() valueName("<video-file>") action { (file, p) =>
      p.copy(file = Some(file))
    } text("Search subtitle for given <video-file>")

    // --imdb <imdb-id>
    opt[String]('i', "imdb") optional() valueName("<imdb-id>") action { (id, p) =>
      p.copy(imdb = Some(id))
    } validate { id =>
      if (id.matches(Params.ImdbId.regex)) success
      else failure(s"<imdb-id> must match ${Params.ImdbId.regex}")
    } text("Search subtitle with given <imdb-id> (e.g. tt0133093)")
    */

    // --interactive
    opt[Unit]('i', "interactive") action { (_, p) =>
      p.copy(interactive = true)
    } text "Interactive-mode: Select downloaded subtitle from options by yourself"

    // files or dirs
    arg[File]("<file or dir>...") unbounded() required() action { (file, p) =>
      p.copy(files = p.files :+ file)
    } validate { file =>
      if (!file.exists)          failure(s"$file does not exist")
      else if (!file.canRead)    failure(s"Cannot read $file")
      else if (file.isDirectory) {
        if (!file.canWrite)      failure(s"Cannot write to directory $file")
        else                     success
      } else if (file.isFile) {
        val parentDir = file.getParentFile
        if (!parentDir.canWrite) failure(s"Cannot write to parent directory $parentDir")
        else                     success
      } else                     failure(s"Unknown $file")
    } text "List of video files or directories (only files without an existing subtitle are processed) to search subtitle(s) for"

    help("help") text "Prints this usage text"

    /*
    opt[File]('o', "out") required() valueName("<file>") action { (x, c) =>
      c.copy(out = x) } text("out is a required file property")
    opt[(String, Int)]("max") action { case ((k, v), c) =>
      c.copy(libName = k, maxCount = v) } validate { x =>
      if (x._2 > 0) success else failure("Value <max> must be >0")
    } keyValueName("<libname>", "<max>") text("maximum count for <libname>")
    opt[Seq[File]]('j', "jars") valueName("<jar1>,<jar2>...") action { (x,c) =>
      c.copy(jars = x) } text("jars to include")
    opt[Map[String,String]]("kwargs") valueName("k1=v1,k2=v2...") action { (x, c) =>
      c.copy(kwargs = x) } text("other arguments")
    opt[Unit]("verbose") action { (_, c) =>
      c.copy(verbose = true) } text("verbose is a flag")
    opt[Unit]("debug") hidden() action { (_, c) =>
      c.copy(debug = true) } text("this option is hidden in the usage text")
    note("some notes.\n")
    help("help") text("prints this usage text")
    arg[File]("<file>...") unbounded() optional() action { (x, c) =>
      c.copy(files = c.files :+ x) } text("optional unbounded args")
    cmd("update") action { (_, c) =>
      c.copy(mode = "update") } text("update is a command.") children(
      opt[Unit]("not-keepalive") abbr("nk") action { (_, c) =>
        c.copy(keepalive = false) } text("disable keepalive"),
      opt[Boolean]("xyz") action { (x, c) =>
        c.copy(xyz = x) } text("xyz is a boolean property"),
      checkConfig { c =>
        if (c.keepalive && c.xyz) failure("xyz cannot keep alive") else success }
      )
      */
  }


  def main(args: Array[String]) {
    val params = parser.parse(args, Params()) match {
      case Some(params) => params
      case None         => sys.exit(1)
    }

    implicit val settings: Settings = {
      import net.ceedubs.ficus.Ficus._
      import net.ceedubs.ficus.readers.ArbitraryTypeReader._
      val configName = "subtitler.conf"
      try {
        val configExists = Seq(configName, "/"+configName).exists(n => Option(this.getClass.getResource(n)).isDefined)
        require(configExists, s"Cannot find subtitler.conf")
        val config = ConfigFactory.parseResources(configName)
        val openSubtitles = config.as[OpenSubtitlesConf]("openSubtitles")
        val languages = config.getAs[List[String]]("languages").getOrElse(Nil)
        Settings(languages, openSubtitles)
      } catch {
        case t: Throwable =>
          System.err.println(s"Error reading $configName: ${t.getMessage}")
          sys.exit(1)
      }
    }

    //testAnsi()
    println(consoleWidth)
    println("sleep")
    Thread.sleep(2000)
    println(consoleWidth)

    dispatch.Http.shutdown()
  }

  case class VideoDir(dir: File, videoCount: Int, videosWithoutSubtitles: List[File]) {
    require(dir.isDirectory, s"$dir is not a directory")
    def videoCountWithoutSubtitles = videosWithoutSubtitles.size
  }
  case class WorkRequest(dirBasedVideos: List[VideoDir], videos: List[File], interactive: Boolean) {
    lazy val distinctVideos = (dirBasedVideos.flatMap(_.videosWithoutSubtitles) ++ videos).distinct
  }
  object WorkRequest {
    def create(params: Params) = {
      val (dirs, files) = params.files.toList.partition(_.isDirectory)
      val videoDirs = dirs.map { dir =>
        val videoFiles = FileUtils.findFiles(dir, true, isVideoFile)
        VideoDir(dir, videoFiles.size, videoFiles.filter(existingSubtitles(_).isEmpty))
      }
      WorkRequest(videoDirs, files, params.interactive)
    }
  }

  case class DownloadResult(file: File, success: Boolean, message: String)
  def downloadSubtitles(files: Seq[File], interactive: Boolean)(implicit s: Settings) = {
    val results = files.foldLeft(List.empty[DownloadResult]) { (acc, file) =>
      if (!interactive) {
        val future: Future[Option[(Subtitle, SubtitleData)]] = OpenSubtitlesAPI.searchSubtitle(file).flatMap {
          case Some(subtitle) =>
            OpenSubtitlesAPI.downloadSubtitles(subtitle).
              map(datas => Some(datas.head))
          case None =>
            Future.successful(None)
        }
        Try(Await.result(future, 1.minute)) match {
          case Success(maybeData) =>
            maybeData match {
              case Some((subtitle, data)) =>
                val basename = FilenameUtils.getBaseName(file.getName)
                val targetFile = new File(file.getParentFile, s"$basename.${subtitle.formatSafe}")
                if (targetFile != file) {
                  val fw = new FileWriter(targetFile)
                  fw.write(data.content)
                  fw.close
                  DownloadResult(file, true, "") :: acc
                } else {
                  DownloadResult(file, false, "Cannot download to $targetFile") :: acc
                }
              case None =>
                DownloadResult(file, false, "No suitable subtitle found") :: acc
            }
          case Failure(err) =>
            DownloadResult(file, false, err.getMessage) :: acc
        }

      } else {
        ???
      }
    }
    println(results)
  }

  def testAnsi() = {
    import org.fusesource.jansi.AnsiConsole
    import org.fusesource.jansi.Ansi._
    import org.fusesource.jansi.Ansi.Color._

    AnsiConsole.systemInstall()
    val lineSize = s"We haz number x".size
    (1 to 5).foreach { i =>
      val c = if (i % 2 == 0) GREEN else Color.RED
      print {
        ansi.eraseLine(Erase.ALL).fg(c).
          a(s"We haz number $i").reset
      }
      Thread.sleep(1000)
      print("\b" * lineSize) // delete lines
    }
  }

  def testStuff()(implicit settings: Settings) = {
    try {
      /*
      val results = OpenSubtitlesAPI.parseSearchResponse(xml.XML.loadFile(new File("/tmp/subtitles.xml")))
      results.foreach(println)
      */

      /*
      val file = videoFiles(1)
      val result = Await.result(OpenSubtitlesAPI.searchSubtitle(file), 10.seconds)
      println(result)
      println(file)
      result.map { subtitle =>
        val res = Await.result(OpenSubtitlesAPI.downloadSubtitles(subtitle.downloadId), 20.seconds)
        println(res)
      }
      */

      val res = Await.result(OpenSubtitlesAPI.downloadSubtitleIds("1954499037", "1954510340"), 20.seconds)
      println(res(0).content)
      println(res)

    } finally {

    }
  }

  def consoleWidth(): Int = {
    import scala.sys.process._
    Try(Seq("bash", "-c", "tput cols 2> /dev/tty").!!.trim.toInt).getOrElse(100)
  }


}

object FileUtils {

  private val VideoExtensions = Set("avi", "mkv", "mp4")
  def isVideoFile(f: File) = f.isFile && VideoExtensions(FilenameUtils.getExtension(f.getName))

  private val SubtitleExtensions = Set("srt", "sub")
  def isSubtitleFile(f: File) = f.isFile && SubtitleExtensions(FilenameUtils.getExtension(f.getName))

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

  def existingSubtitles(f: File): List[File] = {
    val dir = f.getParentFile
    val basename = FilenameUtils.getBaseName(f.getName)
    dir.listFiles.filter(f => isSubtitleFile(f) && f.getName.startsWith(basename)).toList
  }

}
