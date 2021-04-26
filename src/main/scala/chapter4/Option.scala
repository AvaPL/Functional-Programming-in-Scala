package chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def orElse[B >: A](other: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(other)

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(value => if (f(value)) this else None)
}

object Option {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(b => f(a, b)))

  def sequence[A](list: List[Option[A]]): Option[List[A]] =
    traverse(list)(identity)

  // Against clean code but good for practicing functional folds :)
  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] =
    list.map(f).foldRight[Option[List[B]]](Some(Nil))(map2(_, _)(_ :: _))
}

case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]