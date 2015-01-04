package fi.pyppe.subtitler

import play.api.libs.ws.{WSResponse, WSRequestHolder, DefaultWSClientConfig}
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder

import scala.xml.XML

object WSConfig {

  private val clientConfig = new DefaultWSClientConfig()
  private val secureDefaults = new NingAsyncHttpClientConfigBuilder(clientConfig).build()
  private val builder = {
    new com.ning.http.client.AsyncHttpClientConfig.Builder(secureDefaults).
      setCompressionEnabled(true)
  }

  implicit val WSClient = new play.api.libs.ws.ning.NingWSClient(builder.build)

  implicit class WSResponseExtras(res: WSResponse) {
    def asXML() = XML.loadString(res.body)
  }


}
