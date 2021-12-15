package endpoints4s.xhr.thenable

import endpoints4s.algebra.MuxRequest
import endpoints4s.{Decoder, Encoder, xhr}

import scala.scalajs.js

/** @group interpreters
  */
trait MuxEndpoints extends xhr.MuxEndpoints with EndpointsWithCustomErrors {

  class MuxEndpoint[Req <: MuxRequest, Resp, Transport](
      request: Request[Transport],
      response: Response[Transport]
  ) {
    def apply(
        req: Req
    )(implicit
        encoder: Encoder[Req, Transport],
        decoder: Decoder[Transport, Resp]
    ): (js.Thenable[req.Response], js.Function1[Unit, Unit]) = {
      muxPerformXhr(request, response, req)
    }
  }

  def muxEndpoint[Req <: MuxRequest, Resp, Transport](
      request: Request[Transport],
      response: Response[Transport]
  ): MuxEndpoint[Req, Resp, Transport] =
    new MuxEndpoint[Req, Resp, Transport](request, response)

}
