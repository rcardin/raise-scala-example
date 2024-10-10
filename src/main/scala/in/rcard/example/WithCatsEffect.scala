package in.rcard.example

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.*

object WithCatsEffect extends IOApp.Simple {
  override def run: cats.effect.IO[Unit] = {
//    (for {
//      _ <- IO.sleep(500.milliseconds)
//      _ <- IO.raiseError(new RuntimeException("Something went wrong"))
//    } yield ())
//      .timeout(10.second)

    IO.race(
      for {
        _ <- IO.sleep(500.milliseconds)
        value <- IO.pure(43) // IO.raiseError(new RuntimeException("Something went wrong"))
      } yield value,
      for {
        _     <- IO.sleep(1.second)
        value <- IO.pure(42)
      } yield value
    ).map {
      case Left(v)  => println(v)
      case Right(v) => println(v)
    }
  }
}
