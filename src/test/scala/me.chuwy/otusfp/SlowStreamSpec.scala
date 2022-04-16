package me.chuwy.otusfp

import org.specs2.mutable.Specification
import cats.effect.IO
import cats.effect.kernel.Ref
import fs2.text
import org.http4s.Method.GET
import org.http4s.Request
import cats.effect.unsafe.implicits.global
import org.http4s.implicits.http4sLiteralsSyntax


class SlowStreamSpec extends Specification {

  "Slow stream" should {
    "return exact number of chars" in {

      val slowRequest = Request[IO](GET, uri"/slow/10/25/1")

      val ref = Ref[IO].of(0).unsafeRunSync()
      val route = Restful.route(ref)

      val startTime = java.time.Instant.now()

      val slowResponse = route(slowRequest).value.unsafeRunSync().get
      val slowResponseString = slowResponse.body.through(text.utf8.decode).compile.string.unsafeRunSync()

      val stopTime = java.time.Instant.now()

      slowResponseString mustEqual "*".repeat(25)
      startTime.plusSeconds(3) must be_<=(stopTime)

    }
  }
}
