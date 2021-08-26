package chapter12.monad

import chapter12.Identity
import chapter12.applicative1.Applicative
import chapter12.traverse.Traverse
import chapter6.State

trait Monad[F[_]] extends Applicative[F] {
  self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    // Assumes that compose is not implemented via flatMap
    compose(identity[F[A]], f)(fa)
  }

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)

  def flatMapViaJoinAndMap[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    // Assumes that join and map are not implemented via flatMap
    join(map(fa)(f))
  }

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    // Assumes that join and map are not implemented via flatMap
    a => flatMapViaJoinAndMap(f(a))(g)
  }

  def composeM[G[_]](mg: Monad[G])(tg: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
    override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
      self.flatMap(fa)(ga => self.map(tg.traverse(ga)(f)(self))(mg.join))

    override def unit[A](a: => A): F[G[A]] =
      self.unit(mg.unit(a))
  }
}

object Monad {
  val option: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] =
      Some(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  val lazyList: Monad[LazyList] = new Monad[LazyList] {
    override def unit[A](a: => A): LazyList[A] =
      LazyList(a)

    override def flatMap[A, B](fa: LazyList[A])(f: A => LazyList[B]): LazyList[B] =
      fa.flatMap(f)
  }

  val identity: Monad[Identity] = new Monad[Identity] {
    override def flatMap[A, B](fa: A)(f: A => B): B = f(fa)

    override def unit[A](a: => A): A = a
  }

  def state[S]: Monad[({type f[A] = State[S, A]})#f] = new Monad[({type f[A] = State[S, A]})#f] {
    override def unit[A](a: => A): State[S, A] =
      State.unit(a)

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      fa.flatMap(f)
  }
}
