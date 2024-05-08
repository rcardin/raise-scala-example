package in.rcard.raise4s

import java.util.concurrent.{Callable, CompletableFuture, StructuredTaskScope}

trait Suspended {
  val scope: StructuredTaskScope[Any]
}

class VirtualThreadSuspended extends Suspended {
  override val scope: StructuredTaskScope[Any] = new StructuredTaskScope[Any]
}

type suspend[A] = Suspended ?=> A

//class Job[A] {
//  val result = new CompletableFuture[A]()
//
//  def join(): A = result.join()
//}

type Job[A] = CompletableFuture[A]
extension [A](job: Job[A]) def value: A = job.join()

object Sus {

  val sus1: suspend[Int] = 42
  val sus2: suspend[Int] = 43

  def structured[A](block: Suspended ?=> A): Unit = {
    given suspended: Suspended = VirtualThreadSuspended()
    try block
    finally
      suspended.scope.join()
      suspended.scope.close()
  }

//  def fork[T](block: => T)(using suspended: Suspended): Job[T] = {
//    val job = new Job[T]
//    suspended.scope.fork(() => {
//      job.result.complete(block)
//    })
//    job
//  }

  def fork[T](block: => T): Suspended ?=> Job[T] = {
    val result = new CompletableFuture[T]()
    summon[Suspended].scope.fork(() => {
      try {
        result.complete(block)
      } catch {
        case e: Throwable =>
          result.completeExceptionally(e)
      }
    })
    result
  }

  @main def main2(): Unit = {
    structured {
//      val v1: Job[Int] = fork {
//        Thread.sleep(1000)
//        println("First job")
//        42
//      }
      val v2: Job[Int] = fork {
        Thread.sleep(500)
        println("Second job")
        throw RuntimeException("Error")
//        1
      }
      println(v2.value)
    }
  }
}
