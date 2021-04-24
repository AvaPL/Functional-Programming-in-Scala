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

case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]