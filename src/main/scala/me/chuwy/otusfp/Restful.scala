package me.chuwy.otusfp

import cats.effect.{IO, Ref}
import cats.data.ReaderT
import cats.effect.unsafe.implicits.global
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl.io._
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder

import scala.concurrent.duration._
import fs2.Stream
import scala.util._

object PositiveIntVar {
  def unapply (str: String): Option[Int] =
    Try(str.toInt).flatMap(v => Try(
      if (v > 0) v else throw new Exception("Parameter should be positive!")
    )).toOption
}

object PositiveDoubleVar {
  def unapply (str: String): Option[Double] =
    Try(str.toDouble).flatMap(v => Try(
      if (v > 0) v else throw new Exception("Parameter should be positive!")
    )).toOption
}

object Restful {

  case class Environment(config: String, threadPool: Int)

  type MyApp[A] = ReaderT[IO, Environment, A]

  val ourFirstReader: MyApp[String] =
    ReaderT((i: Environment) => IO.pure(i.toString))

  val readerResult: MyApp[Unit] = for {
    _ <- ourFirstReader
    _ <- ourFirstReader
    _ <- ourFirstReader
    _ <- ourFirstReader
  } yield ()

  def getFromDb: IO[String] = IO.pure("Ok")

  def route(cref: Ref[IO, Int]) = HttpRoutes.of[IO] {
    case GET -> Root / "api" / name =>
      getFromDb.flatMap { _ =>
        Ok(s"Hello, $name")
      }
    case GET -> Root / "server" =>
      Forbidden("you have no access")

    case GET -> Root / "counter" =>
    for {
      v <- cref.updateAndGet(_ + 1)
      result <- Ok(s"$v")
    } yield(result)

    case GET -> Root / "slow" / PositiveIntVar(total) / PositiveDoubleVar(rate) =>
          val slowStream = Stream.awakeEvery[IO]((1/rate).second) zipRight Stream("*").repeat.take(total)
          Ok(slowStream)

   }

  def server(cref: Ref[IO, Int]) = BlazeServerBuilder[IO]
    .bindHttp(port = 8080, host = "localhost")
    .withHttpApp(route(cref).orNotFound)
    .resource
}
