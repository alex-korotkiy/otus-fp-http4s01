package me.chuwy.otusfp

import cats.effect.{IO, Ref}
import cats.data.ReaderT
import cats.effect.unsafe.implicits.global
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl.io._
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder

import scala.util._

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

  def slowParamsConverter(strTotal: String, strRate: String): Try[(Int, Double)] = {
    val preResult = Try(strTotal.toInt, strRate.toDouble)
    preResult match {
      case Failure(exception) =>
        preResult
      case Success((total, rate)) =>
        if(total <= 0)
          Failure(new Exception("Total should be positive"))
        if (rate <=0)
          Failure(new Exception("Rate should be positive"))
        preResult
    }
  }

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

    case GET -> Root / "slow" / total / rate => {
      val data = slowParamsConverter(total, rate)
      data match {
        case Failure(exception) =>
          UnprocessableEntity(exception.toString)
        case Success((nTotal, nRate)) =>
          Ok("*".repeat(nTotal))
      }
    }
   }

  def server(cref: Ref[IO, Int]) = BlazeServerBuilder[IO]
    .bindHttp(port = 8080, host = "localhost")
    .withHttpApp(route(cref).orNotFound)
    .resource
}
