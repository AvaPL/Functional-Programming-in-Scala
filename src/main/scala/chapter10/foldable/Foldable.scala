package chapter10.foldable

import chapter10.monoid.Monoid
import chapter3.Tree

trait Foldable[F[_]] {
  def foldRight[A, B](fa: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(fa)(Monoid.endofunction[B])(a => f(a, _))(z)

  def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(fa)(Monoid.endofunction[B])(a => f(_, a))(z)

  def foldMap[A, B](fa: F[A])(monoid: Monoid[B])(f: A => B): B

  def concatenate[A](fa: F[A])(monoid: Monoid[A]): A =
    foldLeft(fa)(monoid.zero)(monoid.op)

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List.empty[A])(_ :: _)
}

object Foldable {
  val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldMap[A, B](fa: List[A])(monoid: Monoid[B])(f: A => B): B =
      fa.map(f).foldLeft(monoid.zero)(monoid.op)

    override def toList[A](fa: List[A]): List[A] = fa
  }

  val indexedSeqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldMap[A, B](fa: IndexedSeq[A])(monoid: Monoid[B])(f: A => B): B =
      Monoid.foldMap(fa, monoid)(f)
  }

  val foldableLazyList: Foldable[LazyList] = new Foldable[LazyList] {
    override def foldMap[A, B](fa: LazyList[A])(monoid: Monoid[B])(f: A => B): B =
      fa.map(f).foldLeft(monoid.zero)(monoid.op)
  }

  val foldableTree: Foldable[Tree] = new Foldable[Tree] {
    override def foldMap[A, B](fa: Tree[A])(monoid: Monoid[B])(f: A => B): B =
      fa.fold(f)(monoid.op)
  }

  val foldableOption: Foldable[Option] = new Foldable[Option] {
    override def foldMap[A, B](fa: Option[A])(monoid: Monoid[B])(f: A => B): B =
      fa.fold(monoid.zero)(f)
  }
}
