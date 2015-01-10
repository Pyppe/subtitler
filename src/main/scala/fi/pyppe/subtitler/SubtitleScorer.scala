package fi.pyppe.subtitler

object SubtitleScorer {

  def scoreAndSortCandidates(targetName: String, options: List[Subtitle])
                            (implicit s: Settings): List[(Subtitle, Double)] = {

    val languageCodePoints: Map[String, Double] = s.languages.zipWithIndex.map {
      case (lang, idx) =>
        (lang.code, (s.languages.size - idx).toDouble / 2)
    }.toMap

    options.groupBy(_.downloadId).flatMap {
      case (id, values) =>
        val count = values.size
        val subtitle = values.head

        Some(fileNameScore(targetName, subtitle.subFileName)).filter(_ > 1).map { nameScore =>
          val score =
            languageCodePoints.getOrElse(subtitle.languageCode, 0.0d) +
              nameScore +
              count - (subtitle.badCount/3)

          subtitle -> score
        }
    }.toList.sortBy(_._2).reverse

  }

  private def fileNameScore(fileName1: String, fileName2: String): Int = {
    def terms(str: String) =
      str.split("""[ .-]""").
        map(_.trim.toLowerCase).
        filter(_.nonEmpty).
        dropRight(1).
        toList

    // TODO: LCS is not enough. For example, if looking subs for
    //    Some.Interesting.Documentary.S01E10.HDTV.x264-DIIPADAAPA.mp4
    // Results:
    // 1. Some.Other.Documentary.S01E10.HDTV.x264-DIIPADAAPA.mp4
    // 2. Some.Interesting.Documentary.S01E10.FOOBAR.mp4
    LCS(terms(fileName1), terms(fileName2)).length
  }

  private def nGrams(words: List[String], n: Int): List[List[String]] = {
    (for {
      i <- 1 to math.min(n, words.size)
    } yield {
      words.sliding(i)
    }).flatten.toList
  }

  // Longest-common-subsequence
  def LCS[T](a: Seq[T], b: Seq[T]): Seq[T] = {
    if (a.isEmpty || b.isEmpty)
      Nil
    else if (a.head == b.head)
      a.head +: LCS(a.tail, b.tail)
    else {
      val case1 = LCS(a.tail, b)
      val case2 = LCS(a, b.tail)
      if (case1.length > case2.length) case1 else case2
    }
  }

}
