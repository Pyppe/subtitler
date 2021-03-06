package fi.pyppe.subtitler

import com.typesafe.config.ConfigFactory
import java.io.{FileWriter, File}
import org.apache.commons.io.{IOUtils, FilenameUtils}
import org.apache.commons.lang3.StringUtils
import org.fusesource.jansi.Ansi
import org.joda.time.{PeriodType, Period}
import org.joda.time.format.PeriodFormatterBuilder
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Main extends Logging {
  import FileUtils._
  import scala.concurrent.duration._
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global

  import org.fusesource.jansi.Ansi._
  import org.fusesource.jansi.Ansi.Color._

  case class Params(showSupportedLanguages: Boolean = false, files: Seq[File] = Nil,
                    interactive: Boolean = false, simulate: Boolean = false,
                    showCache: Boolean = false, clearCache: Boolean = false)
  object Params {
    val ImdbId = """.*(tt[0-9]+).*""".r
  }


  val parser = new scopt.OptionParser[Params]("subtitler") {
    head("subtitler", "1.0")

    // --clear-cache
    opt[Unit]('c', "clear-cache") action { (_, p) =>
      p.copy(clearCache = true)
    } text "Clear cache"

    // --interactive
    opt[Unit]('i', "interactive") action { (_, p) =>
      p.copy(interactive = true)
    } text "Interactive-mode: Select yourself the subtitle to download from available options"

    // --show-cache
    opt[Unit]("show-cache") action { (_, p) =>
      p.copy(showCache = true)
    } text "Show the content of Cache"

    // --simulate
    opt[Unit]('s', "simulate") action { (_, p) =>
      p.copy(simulate = true)
    } text "Simulate-mode: Perform a simulation of events without actually writing subs to disk"

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
    } else if (params.showCache) {
      println(Cache.readPrettyJson().getOrElse("No cache"))
    } else if (params.clearCache) {
      Cache.write(CacheState(Nil))
      println(ansi.fg(GREEN).a("Cache successfully cleared").reset)
    } else {
      val ignoredFiles: Set[File] =
        if (!params.interactive) {
          Cache.read.map(_.ignoredVideos.map(v => new File(v.file)).toSet).
            getOrElse(Set.empty)
        } else Set.empty
      val t = System.currentTimeMillis
      val work = WorkRequest.create(params, ignoredFiles)
      println(preSummary(work))
      val results = downloadSubtitles(work.distinctVideos, params.interactive, params.simulate)
      println(postSummary(results, System.currentTimeMillis - t))
      if (!params.interactive) {
        val skippedFiles = results.filter(_.resultType == ResultType.Skipped).map(_.file).toSet
        Cache.addIgnoredFiles(skippedFiles)
      }
    }

    HttpUtils.http.shutdown()
    dispatch.Http.shutdown()
  }

  case class VideoDir(dir: File, videoCount: Int, videosWithoutSub: List[File], ignoredCount: Int) {
    require(dir.isDirectory, s"$dir is not a directory")
    def videoCountWithoutSubs = videosWithoutSub.size
  }
  case class WorkRequest(dirBasedVideos: List[VideoDir], videos: List[File], interactive: Boolean) {
    lazy val distinctVideos = (dirBasedVideos.flatMap(_.videosWithoutSub) ++ videos).distinct
  }
  object WorkRequest {
    def create(params: Params, ignoredFiles: Set[File]) = {
      val (dirs, files) = params.files.map(_.getCanonicalFile).toList.partition(_.isDirectory)
      val videoDirs = dirs.map { dir =>
        val videoFiles = FileUtils.findFiles(dir, true, isVideoFile)
        val newVideoFiles = videoFiles.filter(existingSubtitles(_).isEmpty)
        val (ignoredVideoFiles, theVideoFiles) = newVideoFiles.partition(ignoredFiles)
        VideoDir(dir, videoFiles.size, theVideoFiles, ignoredVideoFiles.size)
      }
      WorkRequest(videoDirs, files, params.interactive)
    }
  }

  def postSummary(results: List[DownloadResult], duration: Long) = {
    import ResultType._
    val sb = StringBuilder.newBuilder
    val hmsFormatter =
      new PeriodFormatterBuilder().
        appendHours().appendSuffix(" h").
        appendSeparator(" ").
        appendMinutes().appendSuffix(" min").
        appendSeparator(" ").
        printZeroAlways().
        appendSeconds().appendSuffix(" sec").
        toFormatter
    def appendLine(s: Any) = sb append (s.toString + "\n")
    def icon(c: Char, color: Color) = ansi.bold.fg(color).a(s"$c ").reset

    val groups = results.groupBy(_.resultType)
    if (results.nonEmpty) {
      val hms = hmsFormatter.print(new Period(0L, duration, PeriodType.time()))
      val successRate = {
        val successCount: Int = groups.get(Ok).map(_.size).getOrElse(0)
        (successCount.toDouble / results.size * 100).formatted("%.1f") + " %"
      }
      val firstLineParts = List(
        "  Finished subtitle-search for ",
        results.size.toString,
        " subtitles in ",
        hms
      )
      val c = CYAN
      val lineLength = firstLineParts.map(_.length).sum + 2
      appendLine(ansi.fg(c).a("="*lineLength).reset)
      appendLine(ansi.fg(c).a(firstLineParts(0)).bold.a(firstLineParts(1)).boldOff.a(firstLineParts(2)).bold.a(firstLineParts(3)).reset)
      appendLine(ansi.fg(c).a("  Success rate ").bold.a(successRate).reset)
      appendLine(ansi.fg(c).a("  See results below").reset)
      appendLine(ansi.fg(c).a("="*lineLength).reset)
    }
    ResultType.values.foreach { rt =>
      groups.get(rt).foreach { results =>
        val (bg, fg, resultPrintLines: List[Ansi]) = rt match {
          case Ok =>
            val color = GREEN
            val lines = results.sortBy(_.subFile.get.getName).map {
              case DownloadResult(f, Some(subFile), Some(orgSubName), _) =>
                val subFileName = subFile.getName
                val suffix =
                  if (subFileName != orgSubName) s" (from $orgSubName)"
                  else ""
                icon('✓', color).fg(color).a(subFileName).reset.a(suffix)
            }
            (color, WHITE, lines)

          case Skipped =>
            val color = YELLOW
            val lines = results.map(_.file).sortBy(_.getName).map { file =>
              icon('-', color).fg(color).a(file.getName).reset
            }
            (WHITE, BLACK, lines)
          case Error =>
            val color = RED
            val lines = results.sortBy(_.file.getName).map {
              case DownloadResult(f,_,_, Some(error)) =>
                icon('×', color).fg(color).a(f.getName).reset.a(s" ($error)").reset
            }
            (color, WHITE, lines)
        }
        val rtPadding = {
          ResultType.values.map(_.toString.size).max + 1
        }
        appendLine(ansi.bold.bg(bg).fg(fg).a(s" ${results.size} × ").a(StringUtils.rightPad(rt.toString, rtPadding)).reset)
        resultPrintLines foreach { l =>
          appendLine("  " + l)
        }
        appendLine("")
      }
    }
    sb.toString
  }

  def preSummary(work: WorkRequest) = {
    val sb = StringBuilder.newBuilder
    def line(l: String) = sb.append(l + "\n")

    val (count, subtitlesText) = {
      val totalCount = work.dirBasedVideos.map(_.videoCountWithoutSubs).sum + work.videos.size
      totalCount -> (if(totalCount == 1) "subtitle" else "subtitles")
    }

    line(s"Trying to find $count $subtitlesText:")
    work.dirBasedVideos.sortBy(_.dir.toString).foreach { vd =>
      line(s" - ${vd.dir} [total videos = ${vd.videoCount}, videos without subs = ${vd.videoCountWithoutSubs}, skipped (cached) = ${vd.ignoredCount}]")
    }
    work.videos.sortBy(_.toString).foreach { f =>
      line(s" - $f")
    }
    sb.toString
  }

  object ResultType extends Enumeration {
    type Result = Value
    val Ok, Skipped, Error = Value
  }
  case class DownloadResult(file: File, subFile: Option[File], originalSubName: Option[String], errorMessage: Option[String]) {
    import ResultType._
    def resultType: Result = {
      if (subFile.isDefined)
        Ok
      else if (errorMessage.isEmpty)
        Skipped
      else
        Error
    }
  }
  object DownloadResult {
    def success(file: File, subFile: File, originalSubName: String) =
      DownloadResult(file, Some(subFile), Some(originalSubName), None)
    def error(file: File, errorMessage: String) =
      DownloadResult(file, None, None, Some(errorMessage))
    def skipped(file: File) =
      DownloadResult(file, None, None, None)
  }
  def downloadSubtitles(files: Seq[File], interactive: Boolean, simulate: Boolean)
                       (implicit s: Settings): List[DownloadResult] = {

    def downloadAndSaveSubtitle(videoFile: File, subtitleFuture: Future[Option[Subtitle]]): DownloadResult = {
      val future: Future[Option[(Subtitle, SubtitleData)]] =
        subtitleFuture.flatMap {
          case Some(subtitle) => OpenSubtitlesAPI.downloadSubtitles(subtitle).map(ss => Some(ss.head))
          case None           => Future.successful(None)
        }

      Try(Await.result(future, 1.minute)) match {
        case Success(maybeData) =>
          maybeData match {
            case Some((subtitle, data)) =>
              val basename = FilenameUtils.getBaseName(videoFile.getName)
              val targetFile = new File(videoFile.getParentFile, s"$basename.${subtitle.formatSafe}")
              if (targetFile != videoFile) {
                if (!simulate) {
                  val fw = new FileWriter(targetFile)
                  fw.write(data.content)
                  fw.close
                }
                DownloadResult.success(videoFile, targetFile, subtitle.subFileName)
              } else {
                DownloadResult.error(videoFile, s"Cannot download to $targetFile")
              }
            case None =>
              DownloadResult.skipped(videoFile)
          }
        case Failure(err) =>
          DownloadResult.error(videoFile, err.getMessage)
      }
    }

    files.foldLeft(List.empty[DownloadResult]) { (acc, videoFile) =>
      if (!interactive) {
        downloadAndSaveSubtitle(videoFile, OpenSubtitlesAPI.searchSubtitle(videoFile)) :: acc
      } else {
        Try(Await.result(OpenSubtitlesAPI.searchSubtitles(videoFile), 1.minute)) match {
          case Success(options) =>
            val padding = (options.size + 1).toString.length
            val optionsText = options.zipWithIndex.map {
              case ((subtitle, score), idx) =>
                val prefix = s"${ansi.bold.fg(BLUE).a(StringUtils.leftPad(s"${idx+1}", padding) + ")").reset} ${ansi.fgBright(CYAN).a(subtitle.subFileName).reset}"
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

            val boldVideoName = ansi.bold.a(videoFile.getName).reset
            if (options.nonEmpty) {
              println(s"Options for $boldVideoName:")
              println(optionsText)
              val numbers = {
                val skip = ", 0 = skip"
                if (options.size == 1)
                  s"1$skip"
                else
                  s"1-${options.size}$skip"
              }
              println(s"Select subtitle for $boldVideoName to download")
              Try(io.StdIn.readLine(ansi.bold.fg(BLUE).a(s"($numbers)").reset.a(": ").toString).toInt) match {
                case Success(i) if i > 0 && i <= options.size =>
                  downloadAndSaveSubtitle(videoFile, Future.successful(Some(options(i-1)._1))) :: acc
                case _ =>
                  println(s"Skipping ${ansi.bold.a(videoFile.getName).reset}")
                  DownloadResult.skipped(videoFile) :: acc
              }
            } else {
              println(s"No subtitles found for $boldVideoName")
              DownloadResult.skipped(videoFile) :: acc
            }

          case Failure(err) =>
            logger.error(s"Error finding subtitle options for $videoFile")
            DownloadResult.error(videoFile, err.getMessage) :: acc
        }
      }
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
