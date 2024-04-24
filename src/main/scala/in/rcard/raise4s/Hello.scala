package in.rcard.raise4s

import in.rcard.raise4s.Raise.{ensure, ensureNotNull}
import in.rcard.raise4s.RaiseAnyPredef.{raise, succeed}
import in.rcard.raise4s.raises
import ox.{ErrorMode, par, sleep}

import scala.concurrent.duration.*

//case class User(id: String, name: String = "Alice")
//
//sealed trait Error
//case class UserNotFound(id: String) extends Error
//
//def findUserById(id: String): User raises Error =
//  if (id == "42") User(id) else raise(UserNotFound(id))
//
//def findUserByIdWithEx(id: String): User =
//  if (id == "42") User(id) else throw new IllegalArgumentException(s"User not found with id: $id")
//
//def findUserByIdWithNone(id: String): User raises None.type =
//  if (id == "42") User(id, "Alice") else raise(None)
//
//case class NegativeAmount(amount: Double) extends Error
//def convertToUsd(amount: Double, currency: String): Double raises NegativeAmount =
//  if (amount < 0) raise(NegativeAmount(amount))
//  else amount
//
//def convertUnionToEither[E, A](
//    input: E | A
//)(using aTest: TypeTest[Any, A], eTest: TypeTest[Any, E]): Either[E, A] =
//  input match
//    case a: A => Right(a)
//    case e: E => Left(e)

//object MyConversions:
//  given unionTypeToEither[A]: Conversion[String | A, Either[String, A]] with
//    def apply(value: String | A)(using TypeTest[Any, A]): Either[String, A] = {
//      value match
//        case error: String => Left(error)
//        case a: A         => Right(a)
//    }

//case class User(id: String, name: String)
//case class Document(text: String)
//
//sealed trait Error
//case class UserNotFound(id: String) extends Error
//case object EmptyName               extends Error
//
//case object NetworkError
//
//case object Error1
//case object Error2

//def foo(): String raises Error1.type = "foo"
//def bar(): String raises Error2.type = "bar"
//def baz(): String raises Error1.type | Error2.type =
//  val result1 = foo()
//  val result2 = bar()
//  result1 + result2

//def foo(): Int raises String     = { 42 }
//def bar(): Raise[String] ?=> Int = { 42 }
//
//def findUserById(id: String): User raises UserNotFound | NetworkError.type =
//  if id == "42" then User(id, "Alice")
//  else if id == "43" then raise(UserNotFound(id))
//  else raise(NetworkError)
//
//def findDocumentsWithUserName(userName: String): List[Document] raises EmptyName.type = ???
//
//def findDocumentsByUserId(userId: String): List[Document] raises Error =
//  val user      = findUserById(userId)
//  val documents = findDocumentsWithUserName(user.name)
//  documents
//trait Users {
//  def findUserById(id: String): User raises UserNotFound
//}
//val users = new Users {
//  def findUserById(id: String): User raises UserNotFound = ???
//}

//def findUserByIdWithRaise(id: String): User raises UserNotFound =
//  $catch(
//    () => findUserById(id),
//    { case _: NoSuchElementException =>
//      raise(UserNotFound(id))
//    }
//  )

//case class Cart(id: String)
//case class Product(id: String, name: String)
//
//sealed trait CartError
//case class CartNotFound(id: String) extends CartError
//
////def findProductsByCartId(cartId: String): List[Product] raises CartNotFound = ???
////
////def findUsersByIds(ids: List[String]): List[User] raises List[UserNotFound] = ???
//
//def parRaise[E, T1, T2](t1: => T1 raises E, t2: => T2 raises E): (T1, T2) raises E = ???

sealed trait Error
case object NegativeAmount      extends Error
case object BankAccountNotFound extends Error
case object NotEnoughFunds      extends Error

case class BankAccount(val balance: Double)

def withdraw(account: BankAccount, amount: Double): BankAccount raises Error =
  ensure(amount > 0, () => NegativeAmount)
  ensureNotNull(account, () => BankAccountNotFound)
  ensure(account.balance >= amount, () => NotEnoughFunds)
  account.copy(balance = account.balance - amount)

class RaiseErrorMode[E: Raise] extends ErrorMode[E, [T] =>> Raise[E] ?=> T] {

  private val evaluations: scala.collection.mutable.Map[Raise[E] ?=> Any, Either[E, Any]] =
    scala.collection.mutable.Map.empty

  override def isError[T](f: Raise[E] ?=> T): Boolean = {
    evaluations.getOrElseUpdate(f, Raise.either(f)).isLeft
  }

  override def getError[T](f: Raise[E] ?=> T): E = {
    evaluations(f).swap.getOrElse(null.asInstanceOf[E])
  }

  override def getT[T](f: Raise[E] ?=> T): T =
    evaluations(f).getOrElse(null).asInstanceOf[T]

  override def pure[T](t: T): Raise[E] ?=> T = t.succeed

  override def pureError[T](e: E): Raise[E] ?=> T = e.raise[T]
}

@main def helloWorld(): Unit = {

  val result: (Int, String) raises Int =
    par(RaiseErrorMode[Int])(
      {
        sleep(200.millis)
        println("Lambda 1")
        1
      }, {
        sleep(100.millis)
        println("Lambda 2")
        Raise.raise(-1)
      }
    )

//  val resultEither: Either[Int, (Int, String)] =
//    par(EitherMode[Int])(
//      {
//        sleep(200.millis)
//        println("Lambda 1")
//        Right(1)
//      }, {
//        sleep(100.millis)
//        println("Lambda 2")
//        Right("1")
//      }
//    )
//  println(resultEither)

  Raise.fold(
    block = result,
    recover = error => println(s"Error: $error"),
    transform = tuple => {
      println(s"Success: $tuple._1, $tuple._2")
    }
  )
}

//  Raise.fold(
//    block = { result },
//    recover = error => println(s"Error: $error"),
//    transform = (tuple: (Int, Int)) => println(s"Success: $tuple._1")
//  )

//  fold(
//    block = { users.findUserById("42") },
//    catchBlock = ex => println(s"HTTP Status 500: Unexpected error ${ex.getMessage}"),
//    recover = error => println(s"HTTP Status 404: User not found"),
//    transform = user => println(s"HTTP Status 200: $user")
//  )
//
//  val products: List[Product] = recover(
//    { findProductsByCartId("42") },
//    { case CartNotFound(id) => List.empty[Product] }
//  )

//  val maybeUser: Either[Error, User] =
//    either:
//      $catch[User](() => findUserByIdWithEx("42"), {
//        case _: IllegalArgumentException => raise(UserNotFound("42"))
//      })
//
//  fold(
//    block = { findUserById("43") },
//    catchBlock = ex => println(s"Error: $ex"),
//    recover = error => println(s"User not found: $error"),
//    transform = user => println(s"User found: $user")
//  )
//
//  val usdAmount: Double =
//    recover({ convertToUsd(-1, "EUR") }, { case NegativeAmount(amount) => 0.0d })
//
//  val maybeUserNameInUpperCase: Either[Error, String] =
//    either:
//      val user: User = findUserById("42")
//      user.name.toUpperCase
//
//  val userNameInUpperCaseRaiseLambda: Raise[Error] ?=> String = maybeUserNameInUpperCase.bind()
//
//  val one = Right(1)
//  val two = Right(2)
//
//  val three = either {
//    val oneValue = one.bind()
//    val twoValue = two.bind()
//    oneValue + twoValue
//  }
//
//  val maybeUserWithTry: Try[User] =
//    $try:
//      findUserByIdWithEx("42")
//
//  val maybeUserWithOpt: Option[User] =
//    option:
//      findUserByIdWithNone("42")

//  import MyConversions.given
//
//  // Example value of the union type E | A
//  val stringValue: String | Int       = "This is a string"
//  val userNotFoundValue: String | Int = 43
//
//
//  // Convert using the macro
//  val eitherFromString: Either[String, Int] = stringValue
//  val eitherFromInt: Either[String, Int]    = userNotFoundValue
//
//  // Print the results
//  println(eitherFromString) // Should print: Left(This is a string)
//  println(eitherFromInt)    // Should print: Right(42)
