trait Raise[Error]:
  def raise(e: Error): Nothing

def fold[A, B, Error: Raise](
    block: () => B,
    catchBlock: (throwable: Throwable) => B,
    recover: (error: Error) => B,
    transform: (value: A) => B
): B = ???

@main def helloWorld = println("Hello, world")
