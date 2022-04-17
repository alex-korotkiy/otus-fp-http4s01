package me.chuwy.otusfp

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import fs2.text
import org.http4s.Method.GET
import org.http4s.Request
import org.http4s.Uri.uri
import org.specs2.mutable.Specification
import io.circe.generic.auto._
import io.circe.parser.decode
import org.http4s.implicits.http4sLiteralsSyntax

class CounterSpec extends Specification {

  "Counter" should {
    "increment" in {

      val getCounter = Request[IO](GET, uri"/counter")

      val countersIO = for {
        ref <- Ref[IO].of(0)
        route = Restful.route(ref)

        counter1Response <- route(getCounter).value
        counter1String <- counter1Response.get.body.through(text.utf8.decode).compile.string
        counter1 = decode[Counter](counter1String)

        counter2Response <- route(getCounter).value
        counter2String <- counter2Response.get.body.through(text.utf8.decode).compile.string
        counter2 = decode[Counter](counter2String)

      } yield (counter1, counter2)

      val counters = countersIO.unsafeRunSync()
      counters._1 mustEqual Right(Counter(1))
      counters._2 mustEqual Right(Counter(2))

    }
  }


}
