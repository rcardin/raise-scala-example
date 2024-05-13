package in.rcard.raise4s

import in.rcard.raise4s.Raise.raise

import java.util.concurrent.{Callable, CompletableFuture, StructuredTaskScope}

trait Suspended {
  val scope: StructuredTaskScope[Any]
}
type suspend[A] = Suspended ?=> A

class Job[A](private val cf: CompletableFuture[A]) {
  def value: A = cf.join()
}

object Sus {

  val sus1: suspend[Int] = 42
  val sus2: suspend[Int] = 43

  inline def structured[A](inline block: Suspended ?=> A): A = {
    val _scope = new StructuredTaskScope.ShutdownOnFailure()
    given suspended: Suspended = new Suspended {
      override val scope: StructuredTaskScope[Any] = _scope
    }
    try {
      val task = _scope.fork(() => block)
      _scope.join().throwIfFailed(identity)
      task.get()
    } finally {
      _scope.close()
    }
//    val result = Using(new StructuredTaskScope.ShutdownOnFailure()) { _scope =>
//      given suspended: Suspended = new Suspended {
//        override val scope: StructuredTaskScope[Any] = _scope
//      }
//      val blockResult = block
//      _scope.join().throwIfFailed()
//      blockResult
//    }
//    result.get
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
      try result.complete(block)
      catch
        case throwable: Throwable =>
          result.completeExceptionally(throwable)
          throw throwable;
    })
    Job(result)
  }

  @main def main2(): Unit = {

//    val result: Try[Int] = Using(new StructuredTaskScope.ShutdownOnFailure()) { scope =>
//      val value1 = scope.fork(() => {
//        Thread.sleep(1000)
//        println("First job")
//        42
//      });
//
//      val value2 = scope.fork[Int](() => {
//        Thread.sleep(500)
//        println("Second job")
//        throw RuntimeException("Error")
//      });
//      scope.join().throwIfFailed()
//      value1.get() + value2.get()
//    }
//    println(result)

    val res: Int raises String = structured {
      val v1 = fork[Int] {
        Thread.sleep(2000)
        println("First job")
        42
      }
      val v2 = fork[Int] {
        Thread.sleep(1000)
        println("Second job")
        raise("Error")
      }
      v1.value + v2.value
    }
    val result: String | Int = run(res)
    println(result)
  }

  inline def run[Error, A](inline block: Raise[Error] ?=> A): Error | A =
    Raise.fold(block, identity, identity)
}
