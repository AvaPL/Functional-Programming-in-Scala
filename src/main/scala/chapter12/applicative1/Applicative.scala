package chapter12.applicative1

import chapter11.functor.Functor

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
