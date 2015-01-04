package fi.pyppe.subtitler

import java.io.File

import org.scalatest._

class OpenSubtitlesHasherSpec extends FlatSpec with Matchers {

  val breakdanceAvi = new File(getClass.getResource("/breakdance.avi").getPath)

  "OpenSubtitlesHasher" should s"compute hash for ${breakdanceAvi.getName}" in {
    breakdanceAvi.length should be (12909756L)
    OpenSubtitlesHasher.computeHash(breakdanceAvi) should be ("8e245d9679d31e12")
  }
}
