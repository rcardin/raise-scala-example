package in.rcard.example

import scala.util.Random

object AlgEff {

  class Effect[F](val dep: F)
  extension [F, A](eff: Effect[F] ?=> A) {
    inline def map[B](inline f: A => B): Effect[F] ?=> B = eff.flatMap(a => f(a))
    inline def flatMap[B](inline f: A => Effect[F] ?=> B): Effect[F] ?=> B = f(eff)
  }

  type Console  = Console.type
  type Print[A] = Effect[Console] ?=> A

  object Print {
    def print(msg: Any)(using c: Effect[Console]): Unit =
      c.dep.print(msg)

    def println(msg: Any)(using c: Effect[Console]): Unit =
      c.dep.println(msg)

    def run[A](print: Print[A]): A = {
      given c: Effect[Console] = Effect(Console)

      print
    }

    type Sample[A] = Effect[Random] ?=> A
    object Sample {
      def int(using r: Effect[Random]): Int =
        r.dep.nextInt()
        
      def run[A](sample: Sample[A]): A = {
        given r: Effect[Random] = Effect(Random)

        sample
      }
    }

    @main def goEffect(): Unit = {
      
      val program: (Effect[Console], Effect[Random]) ?=> Unit = for {
        _ <- Print.print("Hello, ")
        _ <- Print.println("world!")
        i <- Sample.int
      } yield Print.println(i)

      Print.run(Sample.run(program))
    }
  }
}
