package chapter7

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par(es)

  // `unit` is represented as a function that returns a `UnitFuture`,
  // which is a simple implementation of `Future` that just wraps
  // a constant value. It doesn't use the `ExecutorService` at all.
  // It's always done and can't be cancelled. Its `get` method simply
  // returns the value that we gave it.
  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled: Boolean = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread,
  // in accord with our design choice of having `fork` be the sole function
  // in the API for controlling parallelism. We can always do
  // `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in
  // a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val aFuture = a(es)
      val bFuture = b(es)
      UnitFuture(f(aFuture.get, bFuture.get))
    }

  def map2[A, B, C](a: Par[A], b: Par[B], timeout: Long, units: TimeUnit)(f: (A, B) => C): Par[C] = {
    val timoutNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
    (es: ExecutorService) => {
      val aFuture = a(es)
      val bFuture = b(es)
      val (aValue, elapsedNanos) = getAndMeasureTimeNanos(aFuture, timoutNanos)
      val remainingNanos = timoutNanos - elapsedNanos
      val bValue = getAndMeasureTimeNanos(bFuture, remainingNanos)._1
      UnitFuture(f(aValue, bValue))
    }
  }

  private def getAndMeasureTimeNanos[A](future: Future[A], timeoutNanos: Long): (A, Long) = {
    val startTimeNanos = System.nanoTime()
    val value = future.get(timeoutNanos, TimeUnit.NANOSECONDS)
    val executionTimeNanos = System.nanoTime() - startTimeNanos
    (value, executionTimeNanos)
  }

  def fork[A](par: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call: A = par(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](par: Par[A])(f: A => B): Par[B] =
    map2(par, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def equal[A](es: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(es).get == p2(es).get

  def delay[A](f: => Par[A]): Par[A] =
    es => f(es)

  def choice[A](condition: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(condition).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](par: Par[A]): ParOps[A] = new ParOps(par)

  class ParOps[A](par: Par[A]) {


  }

}