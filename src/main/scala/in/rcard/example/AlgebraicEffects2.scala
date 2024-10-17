package in.rcard.example

import in.rcard.example.AlgebraicEffects2.Effect

import scala.annotation.tailrec
import scala.util.Random
import scala.util.control.{ControlThrowable, NoStackTrace}

object AlgebraicEffects2 {
  type Eff1[F, A] = F ?=> A
  extension [F, A](eff: Eff1[F, A])(using em: Effect[F]) {
    inline def map[B](inline f: A => B): Eff1[F, B] = eff.flatMap(a => f(a)) //em.map(eff)(a =>f(a))
    inline def flatMap[B](inline f: A => Eff1[F, B]): Eff1[F, B] = f(eff) // em.flatMap(eff)(f)
  }
  
  class Effect[F]

//  class EffectMonad[F] extends Monad[[A] =>> Eff1[F, A]] {
//    override def pure[A](x: A): Eff1[F, A] = x
//
//    override def flatMap[A, B](fa: Eff1[F, A])(
//        f: A => Eff1[F, B]
//    ): Eff1[F, B] = f(fa)
//
//    @tailrec
//    final override def tailRecM[A, B](a: A)(
//        f: A => Eff1[F, Either[A, B]]
//    ): Eff1[F, B] = f(a) match {
//      case Left(a)  => tailRecM(a)(f)
//      case Right(b) => b
//    }
//  }

  object Eff1 {
    inline def apply[F, A](inline body: F ?=> A): Eff1[F, A] = body
  }

  type Console  = Console.type
  type Print[A] = Eff1[Console, A]

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
    def apply[A](body: Console ?=> A): Print[A] = Eff1(body)
  }

  case class Raised[E](original: E) extends ControlThrowable with NoStackTrace

  final class Error[-E] {
    def raise(error: E): Nothing =
      throw Raised(error)
  }

  type Raise[E, A] = Eff1[Error[E], A]

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

  type Sample[A] = Eff1[Random, A]

  object Sample {
    // This runs a `Sample[A]` producing a value of type `A`. By default it uses
    // the global random number generator, but the user can pass in a different
    // generator as the first argument.
    def run[A](sample: Sample[A])(using random: Random = scala.util.Random): A = {
      given r: Random = random

      sample
    }

    // Utility to use inside a `Sample[A]` to produce a random `Int`
    def int(using r: Random): Int =
      r.nextInt()

    // Utility to use inside a `Sample[A]` to produce a random `Double`
    def double(using r: Random): Double =
      r.nextDouble()

    // Constructs a `Sample[A]`.
    inline def apply[A](inline body: Random ?=> A): Sample[A] = Eff1[Random, A](body)
  }

  def main(args: Array[String]): Unit = {

//    given em: EffectMonad[Console]       = EffectMonad[Console]()
    given rm: Effect[Error[String]] = Effect[Error[String]]()

//    val p3: Console ?=> Unit = em.map(
//      em.flatMap(Print { Print.println("Hello") })(_ =>
//        Print {
//          Print.println("World")
//        }
//      )
//    )(_ => ())
//
//    println(Print.run(p3))

    val result: (Error[String], Console, Random) ?=> Int = for {
      i <- Raise { 
        Print.println("Hello")
        Sample.int
      }
      j <- Raise {
        Print.println("World")
        Sample.int
      }
    } yield i + j

    println(Raise.run(Print.run(Sample.run(result))))

//    val p1: Print[Unit] =
//      Print { Print.println("Hello") }.flatMap(_ => Print { Print.println("World") }).map(_ => ())

//    val program: Print[Unit] = for {
//      _ <- Print { Print.println("Hello") }
//      _ <- Print { Print.println("World") }
//    } yield ()

//    val p1: Raise[String, Int] = Raise {
//      println("First")
//      42
//    }
//
//    val p2: Raise[String, Int] = p1.flatMap(i =>
//      Raise {
//        println("Second")
//        i * 2
//      }
//    )
//
//    val p3: Raise[String, Int] = p2.map[Int](i => i + 1)
//
//    val p: Raise[String, Int] = for {
//      i: Int <- p1
//      j: Int <- p2
//    } yield i + j

//    val result: String | Int = Raise.run(p)
//    println(result)

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
