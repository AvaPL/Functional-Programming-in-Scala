package chapter12.monad

import chapter12.applicative1.Applicative

trait Monad[F[_]] extends Applicative[F]{
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
}