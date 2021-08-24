package chapter12.applicative1

import chapter10.monoid.Monoid
import chapter11.functor.Functor
import chapter12.{Failure, Success, Validation}

// map2 & unit defines Applicative
trait Applicative[F[_]] extends Functor[F] {
  self =>
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.map(f).foldRight[F[List[B]]](unit(Nil))(map2(_, _)(_ :: _))

  def sequence[A](list: List[F[A]]): F[List[A]] =
    traverse(list)(identity)

  def replicateM[A](m: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(m)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def filterM[A](list: List[A])(f: A => F[Boolean]): F[List[A]] =
    list.foldRight[F[List[A]]](unit(Nil)) {
      case (a, facc) => map2(f(a), facc) {
        case (true, acc) => a :: acc
        case (false, acc) => acc
      }
    }

  def product[G[_]](ag: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
      (self.map2(fa._1, fb._1)(f), ag.map2(fa._2, fb._2)(f))

    override def unit[A](a: => A): (F[A], G[A]) =
      (self.unit(a), ag.unit(a))
  }

  def compose[G[_]](ag: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      self.map2(fa, fb)(ag.map2(_, _)(f))

    override def unit[A](a: => A): F[G[A]] =
      self.unit(ag.unit(a))
  }
}

object Applicative {
  def validation[E]: Applicative[({type f[A] = Validation[E, A]})#f] = new Applicative[({type f[A] = Validation[E, A]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (f: Failure[E], _: Success[_]) => f
        case (_: Success[_], f: Failure[E]) => f
        case (f1: Failure[E], f2: Failure[E]) =>
          val head = f1.head
          val tail = f1.tail ++: f2.head +: f2.tail
          Failure[E](head, tail: _*)
      }
  }
}
