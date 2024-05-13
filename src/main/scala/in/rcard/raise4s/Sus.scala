package in.rcard.raise4s

import in.rcard.raise4s.Raise.raise
import org.slf4j.{Logger, LoggerFactory}

import java.util.concurrent.{Callable, CompletableFuture, StructuredTaskScope}

trait Suspend {
  val scope: StructuredTaskScope[Any]
}
type Suspended[A] = Suspend ?=> A

class Job[A](private val cf: CompletableFuture[A]) {
  def value: A = cf.join()
}

object Sus {

  val LOGGER: Logger = LoggerFactory.getLogger(getClass)

  val sus1: Suspended[Int] = 42
  val sus2: Suspended[Int] = 43

  inline def structured[A](inline block: Suspend ?=> A): A = {
    val _scope = new StructuredTaskScope.ShutdownOnFailure()
    given suspended: Suspend = new Suspend {
      override val scope: StructuredTaskScope[Any] = _scope
    }
    try {
      val task = _scope.fork(() => {
        LOGGER.info(s"${Thread.currentThread()} Starting structured task")
        block
      })
      _scope.join().throwIfFailed(identity)
      task.get()
    } finally {
      _scope.close()
    }
  }

  def fork[T](block: Suspend ?=> T): Suspend ?=> Job[T] = {
    val result = new CompletableFuture[T]()
    summon[Suspend].scope.fork(() => {
      try result.complete(block)
      catch
        case throwable: Throwable =>
          result.completeExceptionally(throwable)
          throw throwable;
    })
    Job(result)
  }

  def findUserById(id: String): Suspended[Int] raises String = {
    Thread.sleep(1000)
    LOGGER.info(s"${Thread.currentThread()} Second job")
    raise("Error")
  }

  @main def main2(): Unit = {

    val res: Int raises String = structured {
      val v1 = fork[Int] {
        Thread.sleep(2000)
        LOGGER.info(s"${Thread.currentThread()} First job")
        42
      }
      val v2 = fork[Int]{
        Thread.sleep(1000)
        LOGGER.info(s"${Thread.currentThread()} Second job")
        raise("Error")
      }
      v1.value + v2.value
    }
    val result: String | Int = Raise.run(res)
    LOGGER.info(s"${Thread.currentThread()} $result")
  }
}
