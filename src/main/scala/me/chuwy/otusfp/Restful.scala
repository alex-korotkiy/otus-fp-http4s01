package me.chuwy.otusfp

import cats.effect.{IO, Ref}
import cats.data.ReaderT
import cats.effect.unsafe.implicits.global
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl.io._
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder

object Restful {

  case class Environment(config: String, threadPool: Int)

  private val counterRef: IO[Ref[IO, Int]] = Ref[IO].of(0)

  def newCounter(): IO[Int] = {
    counterRef.flatMap(r => r.updateAndGet(_+1))
  }

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

  val route = HttpRoutes.of[IO] {
    case GET -> Root / "api" / name =>
      getFromDb.flatMap { _ =>
        Ok(s"Hello, $name")
      }
    case GET -> Root / "server" =>
      Forbidden("you have no access")

    case GET -> Root / "counter" =>
      newCounter().flatMap(v => Ok(s"$v"))

   }

  val server = BlazeServerBuilder[IO]
    .bindHttp(port = 8080, host = "localhost")
    .withHttpApp(route.orNotFound)
    .resource
}
