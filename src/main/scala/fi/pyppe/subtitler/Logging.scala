package fi.pyppe.subtitler

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

trait Logging {
  protected val logger: Logger =
    Logger(LoggerFactory.getLogger(getClass.getName.replaceAll("\\$$", "")))
}
