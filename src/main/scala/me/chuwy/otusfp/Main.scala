package me.chuwy.otusfp

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {
  def run: IO[Unit] = for {
    cr <- Ref[IO].of(0)
    _ <- Restful.server(cr).use { _ =>
      IO.never
    }
  } yield ()
}
