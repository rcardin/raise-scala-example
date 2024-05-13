package in.rcard.raise4s

import in.rcard.raise4s.Bind.value
import in.rcard.raise4s.RaiseAnyPredef.{raise, succeed}
import in.rcard.raise4s.RaiseIterableDef.mapOrAccumulate
import in.rcard.raise4s.raises
import ox.*

import java.util.concurrent.ArrayBlockingQueue
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

case class User(id: String, name: String)
case class Movie(name: String)
case class Actor(name: String)
case class Document(text: String)
//def foo(): Int raises String     = { 42 }
//def bar(): Raise[String] ?=> Int = { 42 }
//
//def findUserById(id: String): User raises UserNotFound | NetworkError.type =
//  if id == "42" then User(id, "Alice")
//  else if id == "43" then raise(UserNotFound(id))
//  else raise(NetworkError)
//
//def findUserById(id: String): Option[User] =
//  if (id == "42") Option(User(id, "Alice")) else Option.empty[User]
//def findDocumentsWithUserName(userName: String): Option[List[Document]] = ???
//
//def printDocumentsByUserId(userId: String): Unit raises None.type = {
//  val user: User                = findUserById(userId).bind()
//  val documents: List[Document] = findDocumentsWithUserName(user.name).bind()
//  documents.foreach(println)
//}

sealed trait Error
case class UserNotFound(id: String) extends Error

def findUserById(id: String): User raises UserNotFound = ???
def findAllById(ids: List[String]): List[User] raises List[UserNotFound] =
  ids.mapOrAccumulate(findUserById)

//def findAllByIdWithRaise(ids: List[String]): List[User] raises UserNotFound =
//  findAllById(ids).value

//object MovieNotFound extends Error
//type MovieNotFound = MovieNotFound.type
//
//def findUserById(id: String): Either[UserNotFound, User] = ???
//def findMovieByUserId(userId: String): Option[Movie]     = ???
//def findActorsByMovie(movie: Movie): List[Actor]         = ???
//
//def findActorsByUserId(userId: String): List[Actor] raises UserNotFound | MovieNotFound = {
//  val user  = findUserById(userId).value
//  val movie = recover(findMovieByUserId(user.id).value) { raise(MovieNotFound) }
//  findActorsByMovie(movie)
//}

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

//
//sealed trait Error
//case class UserNotFound(id: String) extends Error
//
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

//sealed trait Error
//case class UserNotFound(id: String) extends Error

//def findUserById(id: String): User =
//  throw new NoSuchElementException(s"User '$id' not found'")

//def findUserByIdWithRaise(id: String): User raises UserNotFound =
//  findUserById(id).catching { case _: NoSuchElementException =>
//    raise(UserNotFound(id))
//  }

@main def main(): Unit = {}

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

//case class BankAccount(balance: Double)
//
//def withdraw(account: BankAccount, amount: Double): BankAccount raises Error = {
//  ensure(amount > 0, () => NegativeAmount)
//  ensureNotNull(account, () => BankAccountNotFound)
//  ensure(account.balance >= amount, () => NotEnoughFunds)
//  account.copy(balance = account.balance - amount)
//}
//case object NegativeAmount      extends Error
//case object BankAccountNotFound extends Error
//case object NotEnoughFunds      extends Error
//sealed trait Error
//case object UserNotFound extends Error
//case object NetworkError
//
//case class User(id: String, name: String)
//
//trait UserRepository {
//  def findById(id: String): User raises NetworkError.type
//}
//class Users(repository: UserRepository) {
//  def findUserById(id: String): User raises UserNotFound.type = {
//    Raise.withError(
//      { case NetworkError => UserNotFound }, {
//        findUserById(id)
//      }
//    )
//  }
//
//  def findUserById2(id: String): User raises UserNotFound.type | NetworkError.type =
//    findUserById(id)
//}

def raceR[E, F[_], T](em: ErrorMode[E, F])(fs: Seq[() => F[T]]): F[T] =
  unsupervised {
    val result = new ArrayBlockingQueue[Try[F[T]]](fs.size)
    fs.foreach(f => {
      println(s"Forking $f")
      forkUnsupervised({
        val t = Try {
          println("Inside try")
          val res: F[T] = f()
          println(s"Inside try but after: $res")
          res
        }
        println(s"Putting $t in the queue")
        result.put(t)
      })
    })

    @tailrec
    def takeUntilSuccess(failures: Vector[Either[E, Throwable]], left: Int): F[T] =
      println(s"Left $left")
      if left == 0 then
        failures.headOption.getOrElse(throw new NoSuchElementException) match
          case Left(appError) =>
            failures.tail.foldLeft(em.pureError(appError)) {
              case (acc, Left(e))  => em.addSuppressedError(acc, e)
              case (acc, Right(e)) => em.addSuppressedException(acc, e)
            }
          case Right(e) =>
            failures.tail.foreach {
              case Left(ee)  => e.addSuppressed(SecondaryApplicationError(ee))
              case Right(ee) => if e != ee then e.addSuppressed(ee)
            }
            throw e
      else
//        println(s"Results: $result")
        result.tap(println(_)).take() match
          case Success(v) =>
            println(s"Results: $result")
            println(s"Success $v")
            println(s"em.isError ${em.isError(v)}")
            if em.isError(v) then takeUntilSuccess(failures :+ Left(em.getError(v)), left - 1)
            else
              println(s"Returning $v")
              v
          case Failure(e) =>
            println(s"Results: $result")
            println(s"Failure $e")
            takeUntilSuccess(failures :+ Right(e), left - 1)

    takeUntilSuccess(Vector.empty, fs.size)
  }

class RaiseErrorMode[E] extends ErrorMode[E, [T] =>> Raise[E] ?=> T] {

  private val evaluations: scala.collection.mutable.Map[Raise[E] ?=> Any, Either[E, Any]] =
    scala.collection.mutable.Map.empty

  override def isError[T](f: Raise[E] ?=> T): Boolean = {
    if (!evaluations.contains(f)) {
      val result: Either[E, T] = Raise.either(f)
      evaluations.put(f, result)
    }
    evaluations(f).isLeft
  }

  override def getError[T](f: Raise[E] ?=> T): E =
    evaluations(f) match
      case Left(error) => error
      case Right(_)    => throw new IllegalStateException("The raise execution is not an error")

  override def getT[T](f: Raise[E] ?=> T): T =
    evaluations(f) match
      case Right(value) => value.asInstanceOf[T]
      case Left(_)      => throw new IllegalStateException("The raise execution is an error")

  override def pure[T](t: T): Raise[E] ?=> T = t.succeed

  override def pureError[T](e: E): Raise[E] ?=> T = e.raise[T]
}

def raceRaise[E, T](f1: T raises E, f2: T raises E): T raises E =
  raceR(EitherMode[E])(List(() => Raise.either(f1), () => Raise.either(f2))).value

@main def helloWorld(): Unit = {

//  val result1 = race({
//    sleep(200.millis)
//    println("Lambda 1")
//    "42"
//  }, {
//    sleep(100.millis)
//    println("Lambda 2")
//    "43"
//  })
//  println(result1)

//  val list = List(1, 2, 3, 4, 5)
//  val result: List[Int] raises String = list.mapPar(2)(x => {
////    if (x == 2) {
////      Raise.raise("error")
////    }
//    x + 1
//  })

  val result: String raises Int =
    race(
      {
        sleep(200.millis)
        println("Lambda 1")
        "42"
      }, {
        sleep(100.millis)
        println("Lambda 2")
        Raise.raise(-1)
      }
    )

  Raise.fold(
    block = result,
    recover = error => println(s"Error: $error"),
    transform = v => println(s"Transform $v")
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
