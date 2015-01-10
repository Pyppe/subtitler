package fi.pyppe.subtitler

case class Settings(languages: List[Language], openSubtitlesConf: OpenSubtitlesConf)
case class OpenSubtitlesConf(login: String, password: String, useragent: String)
