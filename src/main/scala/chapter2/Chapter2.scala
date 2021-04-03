package chapter2

import scala.annotation.tailrec

object Chapter2 {
  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc1: Int, acc2: Int): Int = {
      if (n == 0) acc1
      else if (n == 1) acc2
      else loop(n - 1, acc2, acc1 + acc2)
    }

    loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    as.sliding(2).forall {
      case Array(x, y) => ordered(x, y)
    }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => (b => f(a, b))

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
