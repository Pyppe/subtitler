package fi.pyppe.subtitler

object SubtitleScorer {

  private val SeasonAndEpisode = """[ .-][sS](\d\d)[eE](\d\d)[ .-]""".r

  def seasonAndEpisode(name: String): Option[(String, String)] =
    SeasonAndEpisode.findAllMatchIn(name).map { m =>
      m.group(1) -> m.group(2)
    }.toList match {
      case List(x) => Some(x)
      case _       => None
    }


  def scoreAndSortCandidates(targetName: String, options: List[Subtitle])
                            (implicit s: Settings): List[(Subtitle, Double)] = {

    val languageCodePoints: Map[String, Double] = s.languages.zipWithIndex.map {
      case (lang, idx) =>
        (lang.code, (s.languages.size - idx).toDouble / 2)
    }.toMap

    val targetSeasonAndEpisode = seasonAndEpisode(targetName)
    options.groupBy(_.downloadId).flatMap {
      case (id, values) =>
        val count = values.size
        val subtitle = values.head

        (targetSeasonAndEpisode, seasonAndEpisode(subtitle.subFileName)) match {
          case (a, b) if a != b =>
            None

          case _ =>
            val ratingScore = subtitle.rating match {
              case n if n > 9.0             =>  1.0
              case n if n != 0.0 & n < 7.0  => -1.0
              case _                        =>  0.0
            }

            Some(fileNameScore(targetName, subtitle.subFileName)).filter(_ > 1).map { nameScore =>
              val score =
                languageCodePoints.getOrElse(subtitle.languageCode, 0.0d) +
                  nameScore +
                  ratingScore +
                  count - (subtitle.badCount/3)

              subtitle -> score
            }
        }
    }.toList.sortBy(_._2).reverse

  }

  private def fileNameScore(target: String, candidate: String): Int = {
    def terms(str: String) =
      str.split("""[ .-]""").
        map(_.trim.toLowerCase).
        filter(_.nonEmpty).
        dropRight(1).
        toList

    def termsWithFactor(n: String) = {
      val ts = terms(n).zipWithIndex
      ts.map { case (t, idx) =>
        t -> (ts.length - idx)
      }
    }

    LCS(termsWithFactor(target), termsWithFactor(candidate)).map(_._2).sum
  }

  private def nGrams(words: List[String], n: Int): List[List[String]] = {
    (for {
      i <- 1 to math.min(n, words.size)
    } yield {
      words.sliding(i)
    }).flatten.toList
  }

  // Longest-common-subsequence
  def LCS(a: Seq[(String, Int)], b: Seq[(String, Int)]): Seq[(String, Int)] = {
    if (a.isEmpty || b.isEmpty)
      Nil
    else if (a.head._1 == b.head._1)
      a.head +: LCS(a.tail, b.tail)
    else {
      val case1 = LCS(a.tail, b)
      val case2 = LCS(a, b.tail)
      if (case1.length > case2.length) case1 else case2
    }
  }
  /*
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
  */

}
