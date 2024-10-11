package in.rcard.example

import scala.util.control.{ControlThrowable, NoStackTrace}

object AlgebraicEffects2 {
  type Eff[F[_], E, A] = F[E] ?=> A
  extension [F[_], E, A](block: Eff[F, E, A]) {
    inline def flatMap[B](inline f: A => Eff[F, E, B]): Eff[F, E, B] =
      val a: A = block
      f(a)

    inline def map[B](inline f: A => B): Eff[F, E, B] =
      val a: A = block
      f(a)
  }

  object Eff {
    inline def apply[F[_], E, A](inline body: F[E] ?=> A): Eff[F, E, A] =
      body
  }

  case class Raised[E](original: E) extends ControlThrowable with NoStackTrace

  final class Error[-E] {
    def raise(error: E): Nothing =
      throw Raised(error)
  }

  type Raise[E, A] = Eff[[X] =>> Error[X], E, A]

  object Raise {
    inline def apply[E, A](inline body: Error[E] ?=> A): Raise[E, A] =
      body

    def raise[A](error: A)(using e: Error[A]): Nothing =
      e.raise(error)

    def run[E, A](block: Raise[E, A]): E | A = {
      try {
        given error: Error[E] = new Error[E]

        block(using error)
      } catch case Raised(original) => original.asInstanceOf[E]
    }
  }

  def main(args: Array[String]): Unit = {

    val p1: Raise[String, Int] = Raise {
      println("First")
      42
    }

    val p2: Raise[String, Int] = p1.flatMap(i =>
      Raise {
        println("Second")
        i * 2
      }
    )

    val result: String | Int = Raise.run(p2)
    println(result)

//    val program: Eff[Error, String, Int] = for {
//      prime <-
//        println("First")
//        2
//      _ <-
//        println("Second")
//        Raise.raise("Booom!")
//      evenPrimes <-
//        println("Third")
//        if (prime % 2 == 0) {
//          prime
//        } else {
//          Raise.raise("Not even")
//        }
//    } yield evenPrimes * 2
  }
}
