package endpoints4s.algebra.client

import java.time.LocalDate
import java.util.UUID

import akka.http.scaladsl.model.DateTime
import com.github.tomakehurst.wiremock.client.WireMock._
import endpoints4s.{Invalid, Valid}
import endpoints4s.algebra.EndpointsTestApi

trait EndpointsTestSuite[T <: EndpointsTestApi] extends ClientTestBase[T] {

  def clientTestSuite() = {

    "Client interpreter" should {

      "return server response for UUID" in {

        val uuid = UUID.randomUUID()
        val response = "wiremockeResponse"

        wireMockServer.stubFor(
          get(urlEqualTo(s"/user/$uuid/description?name=name1&age=18"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        whenReady(call(client.UUIDEndpoint, (uuid, "name1", 18))) {
          _ shouldEqual response
        }
        whenReady(call(client.emptyResponseUUIDEndpoint, (uuid, "name1", 18))) {
          _ shouldEqual (())
        }

      }

      "return server response" in {

        val response = "wiremockeResponse"

        wireMockServer.stubFor(
          get(urlEqualTo("/user/userId/description?name=name1&age=18"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        whenReady(call(client.smokeEndpoint, ("userId", "name1", 18))) {
          _ shouldEqual response
        }
        whenReady(
          call(client.emptyResponseSmokeEndpoint, ("userId", "name1", 18))
        ) {
          _ shouldEqual (())
        }

      }

      "return correct url with optional UUID parameter" in {

        val uuid = UUID.randomUUID()
        val response = "wiremockeResponse"

        wireMockServer.stubFor(
          get(urlEqualTo(s"/user/userId/whatever?id=$uuid"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        wireMockServer.stubFor(
          get(urlEqualTo(s"/user/userId/whatever?id=$uuid&age=18"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        whenReady(call(client.optUUIDQsEndpoint, ("userId", uuid, None))) {
          _ shouldEqual response
        }
        whenReady(call(client.optUUIDQsEndpoint, ("userId", uuid, Some(18)))) {
          _ shouldEqual response
        }

      }

      "return correct url with optional parameter" in {

        val response = "wiremockeResponse"

        wireMockServer.stubFor(
          get(urlEqualTo("/user/userId/whatever?name=name1"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        wireMockServer.stubFor(
          get(urlEqualTo("/user/userId/whatever?name=name1&age=18"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        whenReady(call(client.optQsEndpoint, ("userId", "name1", None))) {
          _ shouldEqual response
        }
        whenReady(call(client.optQsEndpoint, ("userId", "name1", Some(18)))) {
          _ shouldEqual response
        }

      }

      "throw exception when 5xx is returned from server" in {
        wireMockServer.stubFor(
          get(urlEqualTo("/user/userId/description?name=name1&age=18"))
            .willReturn(
              aResponse()
                .withStatus(501)
                .withBody("")
            )
        )

        whenReady(call(client.smokeEndpoint, ("userId", "name1", 18)).failed)(x =>
          x.getMessage shouldBe "Unexpected response status: 501"
        )
        whenReady(
          call(client.emptyResponseSmokeEndpoint, ("userId", "name1", 18)).failed
        )(x => x.getMessage shouldBe "Unexpected response status: 501")
      }

      "throw exception with a detailed error message when 500 is returned from server" in {
        wireMockServer.stubFor(
          get(urlEqualTo("/user/userId/description?name=name1&age=18"))
            .willReturn(
              aResponse()
                .withStatus(500)
                .withBody("[\"Unable to process your request\"]")
            )
        )

        whenReady(call(client.smokeEndpoint, ("userId", "name1", 18)).failed)(x =>
          x.getMessage shouldBe "Unable to process your request"
        )
        whenReady(
          call(client.emptyResponseSmokeEndpoint, ("userId", "name1", 18)).failed
        )(x => x.getMessage shouldBe "Unable to process your request")
      }

      "properly handle joined headers" in {
        val response = UUID.randomUUID().toString
        wireMockServer.stubFor(
          get(urlEqualTo("/joinedHeadersEndpoint"))
            .withHeader("A", equalTo("a"))
            .withHeader("B", equalTo("b"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        whenReady(call(client.joinedHeadersEndpoint, ("a", "b")))(x => x shouldEqual (response))
      }

      "properly handle xmaped headers" in {
        val response = UUID.randomUUID().toString
        wireMockServer.stubFor(
          get(urlEqualTo("/xmapHeadersEndpoint"))
            .withHeader("C", equalTo("11"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        whenReady(call(client.xmapHeadersEndpoint, 11))(x => x shouldEqual (response))
      }

      "properly handle xmaped url" in {
        val response = UUID.randomUUID().toString
        wireMockServer.stubFor(
          get(urlEqualTo("/xmapUrlEndpoint/11"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        whenReady(call(client.xmapUrlEndpoint, "11"))(x => x shouldEqual (response))
      }

      "properly handle xmaped request entites" in {
        val response = UUID.randomUUID().toString
        val dateString = "2018-04-14"
        val date = LocalDate.parse(dateString)
        wireMockServer.stubFor(
          post(urlEqualTo("/xmapReqBodyEndpoint"))
            .withRequestBody(equalTo(dateString))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        whenReady(call(client.xmapReqBodyEndpoint, date))(x => x shouldEqual (response))
      }

      "in case of optional response" should {

        "return Some when response code is 2xx" in {

          val response = "wiremockeResponse"

          wireMockServer.stubFor(
            get(urlEqualTo("/users/1"))
              .willReturn(
                aResponse()
                  .withStatus(200)
                  .withBody(response)
              )
          )

          whenReady(call(client.optionalEndpoint, ()))(
            _ shouldEqual Some(response)
          )

        }

        "return None if server returned 404" in {

          wireMockServer.stubFor(
            get(urlEqualTo("/users/1"))
              .willReturn(
                aResponse()
                  .withStatus(404)
                  .withBody("")
              )
          )

          whenReady(call(client.optionalEndpoint, ()))(_ shouldEqual None)

        }
      }

      "return correct url with trailing slash" in {
        wireMockServer.stubFor(
          get(urlEqualTo(s"/user/"))
            .willReturn(
              aResponse()
                .withStatus(200)
            )
        )

        whenReady(call(client.trailingSlashEndpoint, ())) {
          _ shouldEqual (())
        }
      }

      "Decode response headers" in {
        val response = "foo"
        val etag = s""""${UUID.randomUUID()}""""
        val lastModified = DateTime.now.toRfc1123DateTimeString()
        wireMockServer.stubFor(
          get(urlEqualTo("/versioned-resource"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
                .withHeader("ETag", etag)
                .withHeader("Last-Modified", lastModified)
            )
        )

        whenReady(call(client.versionedResource, ())) { case (entity, cache) =>
          assert(entity == response)
          assert(
            cache.etag.startsWith(etag.dropRight(1))
          ) // Some http client add a “--gzip” suffix
          assert(cache.lastModified == lastModified)
        }
      }

      "Decode optional response headers" in {
        val response = "foo"
        val origin = "*"
        wireMockServer.stubFor(
          get(urlEqualTo("/maybe-cors-enabled"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
                .withHeader("Access-Control-Allow-Origin", origin)
            )
        )

        whenReady(call(client.endpointWithOptionalResponseHeader, ())) { case (entity, header) =>
          (entity, header) shouldEqual ((response, Some(origin)))
        }
      }

      "Decode missing optional response headers" in {
        val response = "foo"
        wireMockServer.stubFor(
          get(urlEqualTo("/maybe-cors-enabled"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody(response)
            )
        )

        whenReady(call(client.endpointWithOptionalResponseHeader, ())) { case (entity, header) =>
          (entity, header) shouldEqual ((response, None))
        }
      }

    }

  }

}
