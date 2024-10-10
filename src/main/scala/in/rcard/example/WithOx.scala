package in.rcard.example

import in.rcard.raise4s.Raise.raise
import in.rcard.raise4s.{Raise, raises}

import scala.concurrent.duration.*

object WithOx {
  @main
  def main(): Unit = {

    val result: Int raises String = ox.race(
      {
        ox.sleep(500.milliseconds)
        Raise.raise("Something went wrong")
      }, {
        ox.sleep(1.second)
        42
      }
    )

    // Prints "42"
    println(Raise.run(result))
  }
}
