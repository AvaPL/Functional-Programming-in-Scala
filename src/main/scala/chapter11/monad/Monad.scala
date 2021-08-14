package chapter11.monad

import chapter11.functor.Functor
import chapter6.State
import chapter7.Nonblocking.Par
import chapter8.Gen
import chapter9.Parser

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](list: List[F[A]]): F[List[A]] =
    traverse(list)(identity)

  def traverse[A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.map(f).foldRight[F[List[B]]](unit(Nil))(map2(_, _)(_ :: _))

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

object Monad {
  val gen: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: A): Gen[A] =
      Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
  }

  val par: Monad[Par] = new Monad[Par] {
    override def unit[A](a: A): Par[A] =
      Par.unit(a)

    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(fa)(f)
  }

  val parser: Monad[Parser] = new Monad[Parser] {
    override def unit[A](a: A): Parser[A] =
      Parser.succeed(a)

    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
      fa.flatMap(f)
  }

  val option: Monad[Option] = new Monad[Option] {
    override def unit[A](a: A): Option[A] =
      Some(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  val lazyList: Monad[LazyList] = new Monad[LazyList] {
    override def unit[A](a: A): LazyList[A] =
      LazyList(a)

    override def flatMap[A, B](fa: LazyList[A])(f: A => LazyList[B]): LazyList[B] =
      fa.flatMap(f)
  }

  val list: Monad[List] = new Monad[List] {
    override def unit[A](a: A): List[A] =
      List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
  }

  def state[S]: Monad[({type f[A] = State[S, A]})#f] = new Monad[({type f[A] = State[S, A]})#f] {
    override def unit[A](a: A): State[S, A] =
      State.unit(a)

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      fa.flatMap(f)
  }
}
