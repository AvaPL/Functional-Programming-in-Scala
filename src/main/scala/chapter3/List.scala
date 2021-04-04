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
    foldRight2(list, toAppend)(Cons(_, _))

  def concat[A](listOfLists: List[List[A]]): List[A] =
    foldLeft(listOfLists, Nil: List[A])(append)

  def add1(ints: List[Int]): List[Int] =
    foldRight2(ints, Nil: List[Int])((int, result) => Cons(int + 1, result))

  def doubleToString(doubles: List[Double]): List[String] =
    foldRight2(doubles, Nil: List[String])((double, result) => Cons(double.toString, result))

  def map[A, B](list: List[A])(f: A => B): List[B] =
    foldRight2(list, Nil: List[B])((element, result) => Cons(f(element), result))

  def filter[A](list: List[A])(f: A => Boolean): List[A] =
    flatMap(list)(element => if (f(element)) List(element) else Nil)

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
    concat(map(list)(f))

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Cons(headA, tailA), Cons(headB, tailB)) => Cons(headA + headB, addPairwise(tailA, tailB))
    case (a, b) => if (a != Nil) a else b
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Cons(headA, tailA), Cons(headB, tailB)) => Cons(f(headA, headB), zipWith(tailA, tailB)(f))
    case _ => Nil
  }

  @tailrec
  def hasSubsequence[A](list: List[A], subsequence: List[A]): Boolean = list match {
    case list if startsWith(list, subsequence) => true
    case Cons(_, tail) => hasSubsequence(tail, subsequence)
    case Nil => false
  }

  @tailrec
  def startsWith[A](list: List[A], subsequence: List[A]): Boolean = (list, subsequence) match {
    case (Cons(headList, tailList), Cons(headSubsequence, tailSubsequence)) if headList == headSubsequence =>
      startsWith(tailList, tailSubsequence)
    case (_, Nil) => true
    case _ => false
  }

  def apply[A](elements: A*): List[A] =
    if (elements.isEmpty) Nil
    else Cons(elements.head, apply(elements.tail: _*))
}