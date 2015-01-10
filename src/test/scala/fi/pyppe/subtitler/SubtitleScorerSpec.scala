package fi.pyppe.subtitler

import org.specs2.mutable.Specification

class SubtitleScorerSpec extends Specification {

  val EnglishLanguage = Language("eng", "en", "English")
  val FinnishLanguage = Language("fin", "fi", "Finnish")

  val EnFi = Settings(List(EnglishLanguage, FinnishLanguage), OpenSubtitlesConf("", "", ""))
  val FiEn = Settings(List(FinnishLanguage, EnglishLanguage), OpenSubtitlesConf("", "", ""))

  "SubtitleScorer" should {

    "take language into account" in {
      val target = "The.Awesome.Documentary.S01E10.1080p.HDTV.x264-FOOBAR.mp4"
      val options = List(
        mockedSub("fi-id", "The Awesome Documentary S01E10 1080p-HDTV.x264-FOOBAR", "fi"),
        mockedSub("en-id", "The Awesome Documentary S01E10 1080p-HDTV.x264-FOOBAR", "en"),
        mockedSub("pl-id", "The Awesome Documentary S01E10 1080p-HDTV.x264-FOOBAR", "pl")
      )
      SubtitleScorer.scoreAndSortCandidates(target, options)(EnFi).map(_._1.downloadId) === List("en-id","fi-id","pl-id")
      SubtitleScorer.scoreAndSortCandidates(target, options)(FiEn).map(_._1.downloadId) === List("fi-id","en-id","pl-id")
    }

    "take negative votes into account" in {
      val name = "The Movie of the Year (2014).mp4"
      def identicalWithBadVoteCount(id: String, c: Int) =
        mockedSub(id, name, "en", badCount = c)

      SubtitleScorer.scoreAndSortCandidates(name, List(
        identicalWithBadVoteCount("1", 1),
        identicalWithBadVoteCount("0", 0),
        identicalWithBadVoteCount("2", 2)
      ))(EnFi).map(_._1.downloadId) === List("0","1","2")
    }

  }

  def mockedSub(id: String, name: String, langCode: String, rating: Double = 0.0d, badCount: Int = 0) =
    Subtitle.empty.copy(
      idSubtitleFile = id,
      subFileName = name,
      languageCode = langCode,
      rating = rating,
      badCount = badCount
    )

}
