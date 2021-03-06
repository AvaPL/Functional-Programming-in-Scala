package chapter10.monoid

import chapter7.Nonblocking.Par
import chapter7.Nonblocking.Par.toParOps

trait Monoid[A] {
  def zero: A

  def op(a1: A, a2: A): A
}

object Monoid {
  val string: Monoid[String] = new Monoid[String] {
    override def zero: String = ""

    override def op(a1: String, a2: String): String = a1 + a2
  }

  def list[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def zero: List[A] = Nil

    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  def option[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
  }

  def endofunction[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = identity

    override def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)
  }

  def concatenate[A](list: List[A], monoid: Monoid[A]): A =
    list.foldLeft(monoid.zero)(monoid.op)

  def foldMap[A, B](list: List[A], monoid: Monoid[B])(f: A => B): B =
    concatenate(list.map(f), monoid)

  def foldRight[A, B](list: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(list, endofunction[B])(a => f(a, _))(z)

  def foldLeft[A, B](list: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(list, endofunction[B])(a => f(_, a))(z)

  def foldMap[A, B](seq: IndexedSeq[A], monoid: Monoid[B])(f: A => B): B = {
    seq.length match {
      case 0 => monoid.zero
      case 1 => f(seq.head)
      case length =>
        val (left, right) = seq.splitAt(length / 2)
        monoid.op(foldMap(left, monoid)(f), foldMap(right, monoid)(f))
    }
  }

  def par[A](monoid: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def zero: Par[A] = Par.unit(monoid.zero)

    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(monoid.op)
  }

  def parFoldMap[A, B](seq: IndexedSeq[A], monoid: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(seq)(f).flatMap(foldMap(_, par(monoid))(Par.lazyUnit(_)))

  def product[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (a.zero, b.zero)

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a1, a2) match {
      case ((a1, b1), (a2, b2)) => (a.op(a1, a2), b.op(b1, b2))
    }
  }

  def function[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = _ => b.zero

    override def op(a1: A => B, a2: A => B): A => B =
      a => b.op(a1(a), a2(a))
  }

  def mapMerge[K, V](monoid: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero: Map[K, V] = Map()

    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, key) =>
        val value = monoid.op(a.getOrElse(key, monoid.zero), b.getOrElse(key, monoid.zero))
        acc.updated(key, value)
      }
  }
}
