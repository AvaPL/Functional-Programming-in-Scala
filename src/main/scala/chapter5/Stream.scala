package chapter5

import chapter5.Stream.{cons, empty}

import scala.annotation.tailrec

trait Stream[+A] {

  def toList: List[A] = {
    @tailrec
    def loop(stream: Stream[A], result: List[A]): List[A] = stream match {
      case Cons(head, tail) => loop(tail(), head() :: result)
      case Empty => result
    }

    loop(this, Nil).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
      case Empty => z
    }

  def exists(predicate: A => Boolean): Boolean =
    foldRight(false)((a, b) => predicate(a) || b)

  @tailrec
  final def find(predicate: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(head, tail) =>
      if (predicate(head())) Some(head()) else tail().find(predicate)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
    case Cons(head, _) if n == 1 => cons(head(), Empty)
    case _ => Empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, tail) if n > 0 => tail().drop(n - 1)
    case _ => this
  }

  def takeWhile(predicate: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (element, result) =>
      if (predicate(element)) cons(element, result) else Empty
    }

  @tailrec
  final def forAll(predicate: A => Boolean): Boolean = this match {
    case Cons(head, tail) => predicate(head()) && tail().forAll(predicate)
    case Empty => true
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((element, _) => Some(element))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((element, result) => cons(f(element), result))

  def filter(predicate: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (element, result) =>
      if (predicate(element)) cons(element, result) else result
    }

  def append[B >: A](toAppend: => B): Stream[B] =
    foldRight(cons(toAppend, Empty))(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])(f(_).concat(_))

  def concat[B >: A](toAppend: => Stream[B]): Stream[B] =
    foldRight(toAppend)(cons(_, _))

  def startsWith[B](s: Stream[B]): Boolean = ???
}

object Stream {
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val headValue = head
    lazy val tailValue = tail
    Cons(() => headValue, () => tailValue)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](elements: A*): Stream[A] =
    if (elements.isEmpty) empty
    else cons(elements.head, apply(elements.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](element: A): Stream[A] = {
    lazy val stream = cons(element, stream)
    stream
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]
