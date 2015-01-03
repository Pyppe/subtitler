package fi.pyppe.subtitler

import java.io.File

case class Settings(watchDirs: List[File], openSubtitlesConf: OpenSubtitlesConf)
case class OpenSubtitlesConf(login: String, password: String, useragent: String)
