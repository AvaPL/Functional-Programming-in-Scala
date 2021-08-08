package chapter10

trait Monoid[A] {
  def zero: A

  def op(a1: A, a2: A): A
}

object Monoid {
  val string: Monoid[String] = new Monoid[String] {
    override def zero: String = ""

    override def op(a1: String, a2: String): String = a1 + a2
  }

  def list[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def zero: List[A] = Nil

    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  def option[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
  }

  def endofunction[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = identity

    override def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)
  }
}
