package chapter12.validation

sealed trait Validation[+E, +A]

case class Success[A](a: A) extends Validation[Nothing, A]

case class Failure[E](head: E, tail: E*) extends Validation[E, Nothing]
