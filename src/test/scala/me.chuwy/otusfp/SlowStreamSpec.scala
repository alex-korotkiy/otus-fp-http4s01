package me.chuwy.otusfp

import org.specs2.mutable.Specification
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.implicits._
import fs2.text
import org.http4s.Method.GET
import org.http4s.Request
import cats.effect.unsafe.implicits.global
import org.http4s.implicits.http4sLiteralsSyntax


class SlowStreamSpec extends Specification {

    "Slow stream" should {
      "return exact number of chars and be slow" in {

        val slowRequest = Request[IO](GET, uri"/slow/10/25/1")

        val slowResponseDataIO = for {
          ref <- Ref[IO].of(0)
          route = Restful.route(ref)

          startTime = java.time.Instant.now()

          slowResponse <- route(slowRequest).value
          slowResponseString <- slowResponse.get.body.through(text.utf8.decode).compile.string

          stopTime = java.time.Instant.now()
          timeDiff = stopTime.toEpochMilli - startTime.toEpochMilli

        } yield (slowResponseString, timeDiff)

        val slowResponseData = slowResponseDataIO.unsafeRunSync()

        slowResponseData._1 mustEqual "*".repeat(25)
        slowResponseData._2.toInt must be_>=(3000)

      }

    "Slow stream" should {
      "validate parameters" in {
        val invalidUrlIO = for {
          ref <- Ref[IO].of(0)
          route = Restful.route(ref)

          invalidUrls = List(
            uri"/slow/a/1/2",
            uri"/slow/-1/2/3",
            uri"/slow/10/0/2",
            uri"/slow/4/2/1.7"
          )
          invalidRequests = invalidUrls.map(url => Request[IO](GET, url))
          invalidRouteResults <- invalidRequests.map(r => route(r).value).sequence

        } yield(invalidRouteResults)

        val routes = invalidUrlIO.unsafeRunSync()
        routes.filter(e => e != None).length mustEqual 0

      }
    }
  }
}
