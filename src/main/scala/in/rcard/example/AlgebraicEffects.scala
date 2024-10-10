package in.rcard.example

import scala.util.control.{ControlThrowable, NoStackTrace}

object AlgebraicEffects {

  case class Raised[E](original: E) extends ControlThrowable with NoStackTrace

  final class Error[-E] {
    def raise(error: E): Nothing =
      throw Raised(error)
  }

  type Raise[E, A] = Error[E] ?=> A
  extension [E, A](block: Raise[E, A]) {
    def flatMap[B](f: A => Raise[E, B]): Raise[E, B] = Raise {
      val a: A = block
      f(a)
    }

    def map[B](f: A => B): Raise[E, B] = Raise {
      val a: A = block
      f(a)
    }

  }
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

  @main def goRaise(): Unit = {

    val third: Raise[String, Int] = Raise {
      println("First")
      2
    }.flatMap(n =>
      Raise[String, Int] {
        println("Second")
        Raise.raise("Booom!")
      }
    ).flatMap(n =>
      Raise[String, Int] {
        println("Third")
        if (n % 2 == 0) {
          n
        } else {
          Raise.raise("Not even")
        }
      }
    ).map(n => n * 2)

    val program: Raise[String, Int] = for {
      prime <- Raise {
        println("First")
        2
      }
      _ <- Raise {
        println("Second")
        Raise.raise("Booom!")
      }
      evenPrimes <- Raise {
        println("Third")
        if (prime % 2 == 0) {
          prime
        } else {
          Raise.raise("Not even")
        }
      }
    } yield prime * 2

    val directStyleProgram: Raise[String, Int] = Raise {
      val prime = Raise {
        println("First")
        2
      }
      Raise {
        println("Second")
        Raise.raise("Booom!")
      }
      val evenPrimes = Raise {
        println("Third")
        if (prime % 2 == 0) {
          prime
        } else {
          Raise.raise("Not even")
        }
      }
      evenPrimes * 2
    }

    val directStyle2: Raise[String, Int] = {
      println("First")
      val prime = 2
      println("Second")
      Raise.raise("Booom!")
      println("Third")
      val evenPrimes = if (prime % 2 == 0) {
        prime
      } else {
        Raise.raise("Not even")
      }
      evenPrimes * 2
    }

    val value = Raise.run(program)
    println(value)
  }
}
