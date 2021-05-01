package chapter5

import chapter5.Stream.{cons, unfold}

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

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(head, tail), n) if n > 1 => Some((head(), (tail(), n - 1)))
      case (Cons(head, _), 1) => Some((head(), (Empty, 0)))
      case _ => None
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

  def takeWhileViaUnfold(predicate: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(head, tail) =>
        val headValue = head()
        Option.when(predicate(headValue))((headValue, tail()))
      case Empty => None
    }

  @tailrec
  final def forAll(predicate: A => Boolean): Boolean = this match {
    case Cons(head, tail) => predicate(head()) && tail().forAll(predicate)
    case Empty => true
  }

  lazy val headOption: Option[A] =
    foldRight(Option.empty[A])((element, _) => Some(element))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((element, result) => cons(f(element), result))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(head, tail) => Some(f(head()), tail())
      case Empty => None
    }

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

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, other)) {
      case (Cons(head1, tail1), Cons(head2, tail2)) =>
        Some((f(head1(), head2()), (tail1(), tail2())))
      case _ => None
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, other)) {
      case (Empty, Empty) => None
      case (stream1, stream2) =>
        Some((stream1.headOption, stream2.headOption), (stream1.drop(1), stream2.drop(1)))
    }

  def startsWith[B >: A](prefix: Stream[B]): Boolean =
    zipAll(prefix).takeWhile(_._2.nonEmpty).forAll {
      case (a, b) => a == b
    }

  lazy val tails: Stream[Stream[A]] =
    scanRight(Stream.empty[A])(cons(_, _))

  def hasSubsequence[B >: A](subsequence: Stream[B]): Boolean =
    tails.exists(_.startsWith(subsequence))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z)) { (element, result) =>
      lazy val resultCached = result
      cons(f(element, resultCached.headOption.get), resultCached)
    }
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

  val onesViaUnfold: Stream[Int] = unfold(())(_ => Some(1, ()))

  def constant[A](element: A): Stream[A] = {
    lazy val stream: Stream[A] = cons(element, stream)
    stream
  }

  def constantViaUnfold[A](element: A): Stream[A] =
    unfold(())(_ => Some(element, ()))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some(i, i + 1))

  val fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = {
      cons(a, loop(b, a + b))
    }

    loop(0, 1)
  }

  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) {
      case (a, b) => Some(a, (b, a + b))
    }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((element, z)) => cons(element, unfold(z)(f))
      case None => Empty
    }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]
