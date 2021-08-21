package chapter12.applicative1

import chapter11.functor.Functor
import chapter12.validation.{Failure, Success, Validation}

trait Applicative[F[_]] extends Functor[F] {
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
