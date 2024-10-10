package in.rcard.example

import zio.*

object WithZio extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    ZIO.raceFirst(
      for {
        _ <- ZIO.sleep(500.milliseconds)
        _ <- ZIO.fail("Something went wrong")
      } yield (),
      List(for {
        _ <- ZIO.sleep(1.second)
        value <- ZIO.succeed(42)
      } yield value)
    ).map(v => println(v))
}
