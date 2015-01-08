package fi.pyppe.subtitler

case class Settings(languages: List[String], openSubtitlesConf: OpenSubtitlesConf)
case class OpenSubtitlesConf(login: String, password: String, useragent: String)
