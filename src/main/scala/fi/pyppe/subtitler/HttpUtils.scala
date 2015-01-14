package fi.pyppe.subtitler

import com.ning.http.client.Response

import scala.xml.{XML, Elem}
import dispatch._
import scala.concurrent.ExecutionContext.Implicits.global

object HttpUtils {
  import scala.concurrent.duration._

  implicit class FiniteDurationExtras(duration: FiniteDuration) {
    def millisInt: Int = duration.toMillis.toInt
  }

  val http = Http.configure(_.setConnectionTimeoutInMs(20.seconds.millisInt).
                              setRequestTimeoutInMs(1.minute.millisInt))

  def postXML(targetUrl: String, xml: Elem): Future[Elem] = {
    val req =
      url(targetUrl).POST.
        setBody(xml.toString).
        setHeader("Content-Type", "application/xml")
    http(req).map { response =>
      require(response.getStatusCode == 200, s"Invalid response code: ${response.getStatusCode}")
      XML.load(response.getResponseBodyAsStream)
    }
  }

  private def printHeaders(response: Response) = {
    import scala.collection.JavaConversions._

    val headers = response.getHeaders.keySet.toList.map { key =>
      key -> response.getHeaders.get(key)
    }.sortBy(_._1)
    println("HEADERS:")
    headers foreach {
      case (key, values) =>
        println(s"$key: ${values.mkString(", ")}")
    }
  }

}
