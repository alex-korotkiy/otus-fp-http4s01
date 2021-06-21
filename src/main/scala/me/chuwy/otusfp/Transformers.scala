package me.chuwy.otusfp

import cats.data.State
import cats.data.EitherT
import cats.data.OptionT
import cats.effect.IO


object Transformers {
  type User = String

  def getUser: IO[Option[User]] = IO.pure(Some("Bob"))
  def getEmail(user: User): IO[Option[String]] = IO.pure(Some("qwerty"))
  def getAccess(user: User, email: String): IO[Option[Int]] = IO.pure(None)

  val userAccess: OptionT[IO, Int] = for {
    user <- OptionT(getUser)
    email <- OptionT(getEmail(user))
    access <- OptionT(getAccess(user, email))
  } yield access

  userAccess.value

  case class World(events: List[String]) {
    def addEvent(event: String): World =
      World(event :: events)
  }
  type Accident = String
  type WorldChange[A] = State[World, A]
  type WorldChangeWithAccident[A] = EitherT[WorldChange, Accident, A]

  def goToSchool: WorldChange[Unit] =
    State { (world: World) => (world.addEvent("go to school"), ()) }
  def goToSchoolT: WorldChangeWithAccident[Unit] =
    EitherT.liftF(goToSchool)

  def goToWork: WorldChange[Unit] =
    State { (world: World) => (world.addEvent("go to work"), ()) }
  def goToWorkT: WorldChangeWithAccident[Unit] =
    EitherT.liftF(goToWork)

  def carAccident: WorldChangeWithAccident[Unit] =
    EitherT.fromEither(Left("car accident"))

  val worldResult: WorldChangeWithAccident[Unit] = for {
    _ <- goToSchoolT
    _ <- carAccident
    _ <- goToWorkT
  } yield ()

  val finalResult = worldResult.value.run(World(Nil)).value





}
