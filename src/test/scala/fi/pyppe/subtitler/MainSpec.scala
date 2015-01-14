package fi.pyppe.subtitler

import java.io.File

import org.specs2.mutable.Specification

class MainSpec extends Specification {
  import Main.DownloadResult
  import Main.ResultType._

  val Suffix = s"HDTV.x64-TESTERS"
  def f(n: String, ext: String) = new File(s"/tmp/$n.$Suffix.$ext")

  "Main" should {
    "print meaningful post-summary" in {
      val results = List(
        DownloadResult.success(f("Unit.Testing.S01E10", "mp4"),
                               f("Unit.Testing.S01E10", "sub"),
                               s"Unit.Testing.S01E10.$Suffix.srt"),
        DownloadResult.success(f("Unit.Testing.S01E11", "mp4"),
                               f("Unit.Testing.S01E11", "sub"),
                               s"Unit.Testing.S01E11.$Suffix.srt"),
        DownloadResult.skipped(f("Not.Found.Movie.2019.720p", "avi")),
        DownloadResult.error(f("The.Most.Unfortunates.1997.1080p", "mkv"), s"Some error message here")
      )
      val summary = Main.postSummary(results)
      println(summary)
      summary.nonEmpty === true
    }
  }

}
