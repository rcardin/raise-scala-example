import scala.util.control.{ControlThrowable, NoStackTrace}

trait Raise[-Error]:
  def raise(e: Error): Nothing

class DefaultRaise extends Raise[Any]:
  def raise(e: Any): Nothing = throw Raised(e)

private class Raised[Error](val original: Error) extends ControlThrowable with NoStackTrace

def raise[Error](e: Error)(using raise: Raise[Error]): Nothing = raise.raise(e)

def fold[A, B, Error](
    block: Raise[Error] ?=> () => A,
    catchBlock: (throwable: Throwable) => B,
    recover: (error: Error) => B,
    transform: (value: A) => B
): B =
  given raise: Raise[Error] = new DefaultRaise
  try transform(block())
  catch
    case e: Raised[Error] => recover(e.original)
    case e: Throwable     => catchBlock(e)
end fold

sealed trait Error
case object MyError extends Error

@main def helloWorld(): Unit =
  val result: String = fold(
    () => 42,
    throwable => "Error: " + throwable.getMessage,
    error => "Error: " + error,
    value => "Value: " + value
  )
  println(result)

  val result2: String = fold(
    () => raise(MyError),
    throwable => "Error: " + throwable.getMessage,
    error => "Error: " + error,
    value => "Value: " + value
  )
  println(result2)

  val result3: String = fold(
    () => throw new RuntimeException("Boom!"),
    throwable => "Error: " + throwable.getMessage,
    error => "Error: " + error,
    value => "Value: " + value
  )
  println(result3)
