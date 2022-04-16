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

      val ref = Ref[IO].of(0).unsafeRunSync()
      val route = Restful.route(ref)

      val counter1Response = route(getCounter).value.unsafeRunSync().get
      val counter1String = counter1Response.body.through(text.utf8.decode).compile.string.unsafeRunSync()
      val counter1 = decode[Counter](counter1String)

      val counter2Response = route(getCounter).value.unsafeRunSync().get
      val counter2String = counter2Response.body.through(text.utf8.decode).compile.string.unsafeRunSync()
      val counter2 = decode[Counter](counter2String)

      counter1 mustEqual Right(Counter(1))
      counter2 mustEqual Right(Counter(2))

    }
  }


}
