package in.rcard.example

import cats.*
import in.rcard.raise4s.Raise

object WithCats {
  def attemptDivide(x: Int, y: Int): Either[String, Int] = {
    if (y == 0) Left("divisor is zero")
    else {
      Right(x / y)
    }
  }

  def attemptDivideApplicativeError[F[_]](x: Int, y: Int)(implicit
      ae: ApplicativeError[F, String]
  ): F[Int] = {
    if (y == 0) ae.raiseError("divisor is error")
    else {
      ae.pure(x / y)
    }
  }

  class RaiseMonadError[Error] extends MonadError[[A] =>> Raise[Error] ?=> A, Error] {
    override def raiseError[A](e: Error): Raise[Error] ?=> A = Raise.raise(e)
    override def handleErrorWith[A](fa: Raise[Error] ?=> A)(
        f: Error => Raise[Error] ?=> A
    ): Raise[Error] ?=> A = Raise.recover(fa) { error =>
      f(error)
    }
    override def pure[A](x: A): Raise[Error] ?=> A = x
    override def ap[A, B](ff: Raise[Error] ?=> A => B)(fa: Raise[Error] ?=> A): Raise[Error] ?=> B =
      ff(fa)

    override def flatMap[A, B](fa: Raise[Error] ?=> A)(
        f: A => Raise[Error] ?=> B
    ): Raise[Error] ?=> B = f(fa)

    override def tailRecM[A, B](a: A)(f: A => Raise[Error] ?=> Either[A, B]): Raise[Error] ?=> B =
      f(a) match {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => b
      }
  }

//  @main
//  def main(): Unit = {
//
//    given RaiseMonadError[String] = RaiseMonadError[String]()
//
//    type OnError[A] = Raise[String] ?=> A
//    val e: OnError[Int] = attemptDivideApplicativeError[OnError](30, 0)
//
//    println(Raise.run(e))
//  }
}
