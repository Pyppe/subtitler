package fi.pyppe.subtitler

import java.io.File

import org.specs2.mutable.Specification

class OpenSubtitlesHasherSpec extends Specification {

  val breakdanceAvi = new File(getClass.getResource("/breakdance.avi").getPath)

  "OpenSubtitlesHasher" should {
    s"compute hash for ${breakdanceAvi.getName}" in {
      breakdanceAvi.length === 12909756L
      OpenSubtitlesHasher.computeHash(breakdanceAvi) === "8e245d9679d31e12"
    }
  }

}
