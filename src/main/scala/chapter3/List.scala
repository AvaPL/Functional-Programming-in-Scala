package chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](list: List[A])(newHead: A): Cons[A] =
    Cons(newHead, tail(list))

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] =
    if (n <= 0) list
    else drop(tail(list), n - 1)

  @tailrec
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => list
  }

  def init[A](list: List[A]): List[A] = list match {
    case Cons(head, tail) if tail != Nil => Cons(head, init(tail))
    case _ => Nil
  }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def sum2(ints: List[Int]): Int =
    foldLeft(ints, 0)((x, y) => x + y)

  def product2(doubles: List[Double]): Double =
    foldLeft(doubles, 1.0)(_ * _)

  def length[A](list: List[A]): Int =
    foldLeft(list, 0)((length, _) => length + 1)

  @tailrec
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, Nil: List[A])((tail, head) => Cons(head, tail))

  def foldRight2[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(list), z)((b, a) => f(a, b))

  def append[A](list: List[A], toAppend: List[A]): List[A] =
    foldRight(list, toAppend)(Cons(_, _))

  def concat[A](listOfLists: List[List[A]]): List[A] =
    foldLeft(listOfLists, Nil: List[A])(append)

  def apply[A](elements: A*): List[A] =
    if (elements.isEmpty) Nil
    else Cons(elements.head, apply(elements.tail: _*))
}