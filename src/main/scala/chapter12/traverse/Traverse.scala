package chapter12.traverse

import chapter10.monoid.Monoid
import chapter12.{Const, Identity}
import chapter12.applicative1.Applicative
import chapter12.monad.Monad
import chapter3.{Branch, Leaf, Tree}
import chapter6.State

trait Traverse[F[_]] {
  self =>
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(f(_): Identity[B])(Monad.identity)

  def foldMap[T, U](fa: F[T])(monoid: Monoid[U])(f: T => U): U =
    traverse(fa)(f(_): Const[U, T])(Applicative(monoid))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.state)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b).run(s)

  def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, list) => (list.head, list.tail))._1

  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit ag: Applicative[G], ah: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(ag.product(ah))

  def compose[G[_]](tg: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
    override def traverse[H[_] : Applicative, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
      self.traverse(fa)(tg.traverse(_)(f))
  }
}

object Traverse {
  val list: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[List[B]] = {
      import applicative.{unit, map2}
      fa.map(f).foldRight(unit(List.empty[B]))(map2(_, _)(_ :: _))
    }
  }

  val option: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Option[B]] = {
      import applicative.unit
      fa.map(f).fold(unit(Option.empty[B]))(applicative.map(_)(Some(_)))
    }
  }

  val tree: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Tree[B]] = {
      import applicative.map2
      fa.map(f).fold(applicative.map(_)(Leaf(_): Tree[B]))(map2(_, _)(Branch(_, _)))
    }
  }
}
