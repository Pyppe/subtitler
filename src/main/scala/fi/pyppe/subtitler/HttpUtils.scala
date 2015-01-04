package fi.pyppe.subtitler

import scala.xml.{XML, Elem}
import dispatch._
import scala.concurrent.ExecutionContext.Implicits.global

object HttpUtils {

  def postXML(targetUrl: String, xml: Elem): Future[Elem] = {
    val req =
      url(targetUrl).POST.
        setBody(xml.toString).
        setHeader("Content-Type", "application/xml")
    Http(req).map { response =>
      require(response.getStatusCode == 200, s"Invalid response code: ${response.getStatusCode}")
      XML.load(response.getResponseBodyAsStream)
    }
  }

}
