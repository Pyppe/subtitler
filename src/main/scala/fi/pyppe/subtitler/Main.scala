package fi.pyppe.subtitler

import com.typesafe.config.ConfigFactory
import java.io.{FileWriter, File}
import org.apache.commons.io.{IOUtils, FilenameUtils}
import org.apache.commons.lang3.StringUtils
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Main {
  //import WSConfig._
  import FileUtils._
  import scala.concurrent.duration._
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global

  import org.fusesource.jansi.Ansi._
  import org.fusesource.jansi.Ansi.Color._

  case class Params(showSupportedLanguages: Boolean = false, files: Seq[File] = Nil, interactive: Boolean = false)
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

    // --supported-langs
    opt[Unit]('l', "supported-langs") action { (_, p) =>
      p.copy(showSupportedLanguages = true)
    } text "Show list of supported languages"

    // files or dirs
    arg[File]("<file or dir>...") unbounded() optional() action { (file, p) =>
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
    org.fusesource.jansi.AnsiConsole.systemInstall()

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
        val languages = config.getAs[List[String]]("languages").getOrElse(Nil).map { lang =>
          Languages.find(lang).getOrElse {
            throw new IllegalArgumentException(s"$lang is not a valid language")
          }
        }
        Settings(languages, openSubtitles)
      } catch {
        case t: Throwable =>
          Console.err.println(s"Error reading $configName: ${t.getMessage}")
          sys.exit(1)
      }
    }

    if (params.showSupportedLanguages) {
      Console.println(ansi.fg(GREEN).a("Supported languages:").reset)
      val sortedLanguages = Languages.Available.toList.sortBy(_.name)
      def languageTokenLength(l: Language) = l.name.size + l.code.size + 3
      val MaxLanguageWidth = sortedLanguages.map(l => languageTokenLength(l) + 1).max
      def ansiName(l: Language) = ansi.a(s"${l.name} (${ansi.fg(CYAN).a(l.code).reset})")
      consoleWidth.toOption match {
        case Some(terminalWidth) if terminalWidth > 90 =>
          sortedLanguages.zipWithIndex.foldLeft(0) { case (cursorWidth, (lang, idx)) =>
            val padding = " "*(MaxLanguageWidth - languageTokenLength(lang))
            if (MaxLanguageWidth + cursorWidth > terminalWidth) {
              Console.print("\n")
              Console.print(ansiName(lang).a(padding))
              MaxLanguageWidth
            } else {
              Console.print(ansiName(lang).a(padding))
              cursorWidth + MaxLanguageWidth
            }
          }
          Console.print("\n")

        case _ =>
          Console.println(sortedLanguages.map(ansiName).mkString(", "))
      }
    } else {
      val work = WorkRequest.create(params)
      Console.println(summary(work))
      downloadSubtitles(work.distinctVideos, params.interactive)
    }


    dispatch.Http.shutdown()
  }

  case class VideoDir(dir: File, videoCount: Int, videosWithoutSub: List[File]) {
    require(dir.isDirectory, s"$dir is not a directory")
    def videoCountWithoutSubs = videosWithoutSub.size
  }
  case class WorkRequest(dirBasedVideos: List[VideoDir], videos: List[File], interactive: Boolean) {
    lazy val distinctVideos = (dirBasedVideos.flatMap(_.videosWithoutSub) ++ videos).distinct
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

  def summary(work: WorkRequest) = {
    val sb = StringBuilder.newBuilder
    def line(l: String) = sb.append(l + "\n")

    val (count, subtitlesText) = {
      val totalCount = work.dirBasedVideos.map(_.videoCountWithoutSubs).sum + work.videos.size
      totalCount -> (if(totalCount == 1) "subtitle" else "subtitles")
    }

    line(s"Trying to find $count $subtitlesText:")
    work.dirBasedVideos.sortBy(_.dir.toString).foreach { vd =>
      line(s" - ${vd.dir} [total videos = ${vd.videoCount}, videos without subs = ${vd.videoCountWithoutSubs}]")
    }
    work.videos.sortBy(_.toString).foreach { f =>
      line(s" - $f")
    }
    sb.toString
  }

  case class DownloadResult(file: File, subFile: Option[File], originalSubName: Option[String], errorMessage: Option[String])
  object DownloadResult {
    def success(file: File, subFile: File, originalSubName: String) =
      DownloadResult(file, Some(subFile), Some(originalSubName), None)
    def error(file: File, errorMessage: String) =
      DownloadResult(file, None, None, Some(errorMessage))
  }
  def downloadSubtitles(files: Seq[File], interactive: Boolean)(implicit s: Settings) = {
    val results = files.foldLeft(List.empty[DownloadResult]) { (acc, videoFile) =>
      if (!interactive) {
        val future: Future[Option[(Subtitle, SubtitleData)]] = OpenSubtitlesAPI.searchSubtitle(videoFile).flatMap {
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
                val basename = FilenameUtils.getBaseName(videoFile.getName)
                val targetFile = new File(videoFile.getParentFile, s"$basename.${subtitle.formatSafe}")
                if (targetFile != videoFile) {
                  val fw = new FileWriter(targetFile)
                  fw.write(data.content)
                  fw.close
                  DownloadResult.success(videoFile, targetFile, subtitle.subFileName) :: acc
                } else {
                  DownloadResult.error(videoFile, s"Cannot download to $targetFile") :: acc
                }
              case None =>
                DownloadResult.error(videoFile, "No suitable subtitle found") :: acc
            }
          case Failure(err) =>
            DownloadResult.error(videoFile, err.getMessage) :: acc
        }

      } else {
        Try(Await.result(OpenSubtitlesAPI.searchSubtitles(videoFile), 1.minute)) match {
          case Success(options) =>
            val padding = (options.size + 1).toString.length
            val optionsText = options.zipWithIndex.map {
              case ((subtitle, score), idx) =>
                val prefix = s"${ansi.bold.fg(BLUE).a(StringUtils.leftPad(s"${idx+1})", padding)).reset} ${ansi.fgBright(CYAN).a(subtitle.subFileName).reset}"
                val suffix: String = {
                  val dataLines = List(
                    ("score", Some(score.toString)),
                    ("lang", Some(subtitle.languageCode)),
                    ("rating", Option(subtitle.rating).filter(_ > 0.0).map(_.toString)),
                    ("negative votes", Option(subtitle.badCount).filter(_ > 0).map(_.toString))
                  )
                  dataLines.collect {
                    case (key, Some(value)) =>
                      Some(s"$key = $value")
                    case _ => None
                  }.flatten.toList match {
                    case Nil => ""
                    case items => items.mkString(" [", ", ", "]")
                  }
                }
                s"  $prefix$suffix"
            }.mkString("\n")

            if (options.nonEmpty) {
              Console.println(s"Options for ${ansi.bold.a(videoFile.getName).reset}:")
              Console.println(optionsText)
              val numbers = {
                val skip = ", 0 = skip"
                if (options.size == 1)
                  s"1$skip"
                else
                  s"1-${options.size}$skip"
              }
              Console.println(
                ansi.a(s"Select subtitle for ").
                  bold.a(videoFile.getName).reset.a(" to download")
              )
              Try(io.StdIn.readLine(ansi.bold.fg(BLUE).a(s"($numbers)").reset.a(": ").toString).toInt) match {
                case Success(i) if i > 0 && i <= options.size =>
                  throw new NotImplementedError(s"TODO: Download the ${options(i-1)._1.subFileName}")
                case _ =>
                  Console.println(s"Skipping ${ansi.bold.a(videoFile.getName).reset}")
              }
            }
            //DownloadResult.success(videoFile, ) :: acc
            ???

          case Failure(err) =>
            ???
        }
      }
    }
    println(results)
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

  def consoleWidth(): Try[Int] = {
    import scala.sys.process._
    Try(Seq("bash", "-c", "tput cols 2> /dev/tty").!!.trim.toInt)
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
