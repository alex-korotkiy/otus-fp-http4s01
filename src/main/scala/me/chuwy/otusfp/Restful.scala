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
      case Failure(_) =>
        preResult
      case Success((total, rate)) =>
        val m1 = if (total > 0) "" else "Total should be positive"
        val m2 = if (rate > 0) "" else "Rate should be positive"
        val errorMsg = List(m1, m2).filter(_ != "").mkString("\r\n")
        if(errorMsg=="")
          preResult
        else
          Failure(new Exception(errorMsg))
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
          val slowStream = Stream.awakeEvery[IO]((1/nRate).second) zipRight Stream("*").repeat.take(nTotal)
          Ok(slowStream)
      }
    }
   }

  def server(cref: Ref[IO, Int]) = BlazeServerBuilder[IO]
    .bindHttp(port = 8080, host = "localhost")
    .withHttpApp(route(cref).orNotFound)
    .resource
}
