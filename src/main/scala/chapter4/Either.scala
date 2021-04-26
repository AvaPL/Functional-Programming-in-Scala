package chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this.flatMap(a => Right(f(a)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case left@Left(_) => left
  }

  def orElse[EE >: E, B >: A](other: => Either[EE, B]): Either[EE, B] = this match {
    case right@Right(_) => right
    case Left(_) => other
  }
}

object Either {

  def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] =
    a.flatMap(a => b.map(b => f(a, b)))

  def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    list.map(f).foldRight[Either[E, List[B]]](Right(Nil))((element, acc) => map2(element, acc)(_ :: _))

  def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] =
    traverse(list)(identity)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]