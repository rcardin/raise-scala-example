package in.rcard.example

import scala.util.control.{ControlThrowable, NoStackTrace}

object AlgebraicEffects {

  type Console = Console.type

  type Print[A] = Console ?=> A
  extension [A](print: Print[A]) {

    /** Insert a prefix before `print` */
    def prefix(first: Print[Unit]): Print[A] =
      Print {
        first
        print
      }

    /** Use red foreground color when printing */
    def red: Print[A] =
      Print {
        Print.print(Console.RED)
        val result = print
        Print.print(Console.RESET)
        result
      }
  }

  object Print {
    def print(msg: Any)(using c: Console): Unit =
      c.print(msg)

    def println(msg: Any)(using c: Console): Unit =
      c.println(msg)

    def run[A](print: Print[A]): A = {
      given c: Console = Console

      print
    }

    /** Constructor for `Print` values */
    inline def apply[A](inline body: Console ?=> A): Print[A] =
      body
  }

  case class Raised[E](original: E) extends ControlThrowable with NoStackTrace

  final class Error[-E] {
    def raise(error: E): Nothing =
      throw Raised(error)
  }

  type Raise[E, A] = Error[E] ?=> A
  extension [E, A](block: Raise[E, A]) {
    inline def flatMap[B](inline f: A => Raise[E, B]): Raise[E, B] = Raise {
      val a: A = block
      f(a)
    }

    inline def map[B](inline f: A => B): Raise[E, B] = Raise {
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

    val third: Error[String] ?=> Console ?=> Int = {
      Print.println("First")
      2
    }.flatMap(n =>
      Print.println("Second")
      Raise.raise("Booom!")
    ).flatMap((n: Int) =>
      Print.println("Third")
      if (n % 2 == 0) {
        n
      } else {
        Raise.raise("Not even")
      }
    ).map(n => n * 2)

    val program: (Error[String], Console) ?=> Int = for {
      prime <-
        Print.println("First")
        2
      _ <-
        Print.println("Second")
        Raise.raise("Booom!")
      evenPrimes <-
        Print.println("Third")
        if (prime % 2 == 0) {
          prime
        } else {
          Raise.raise("Not even")
        }
    } yield evenPrimes * 2

    val value = Raise.run(Print.run(program))
    println(value)
  }
}

//    val directStyleProgram: Raise[String, Int] = Raise {
//      val prime = Raise {
//        println("First")
//        2
//      }
//      Raise {
//        println("Second")
//        Raise.raise("Booom!")
//      }
//      val evenPrimes = Raise {
//        println("Third")
//        if (prime % 2 == 0) {
//          prime
//        } else {
//          Raise.raise("Not even")
//        }
//      }
//      evenPrimes * 2
//    }
//
//    val directStyle2: Raise[String, Int] = {
//      println("First")
//      val prime = 2
//      println("Second")
//      Raise.raise("Booom!")
//      println("Third")
//      val evenPrimes = if (prime % 2 == 0) {
//        prime
//      } else {
//        Raise.raise("Not even")
//      }
//      evenPrimes * 2
//    }
