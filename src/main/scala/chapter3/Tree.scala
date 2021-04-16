package chapter3

sealed trait Tree[+A] {

  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(left, right) => left.size + right.size + 1
  }

  def depth: Int = this match {
    case Leaf(_) => 1
    case Branch(left, right) => left.depth.max(right.depth) + 1
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))
  }

  def fold[B](f: A => B)(g: (B, B) => B): B = this match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(left.fold(f)(g), right.fold(f)(g))
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Branch {

  def apply[A](left: A, right: A): Branch[A] = Branch(Leaf(left), Leaf(right))

  def apply[A](left: Tree[A], right: A): Branch[A] = Branch(left, Leaf(right))

  def apply[A](left: A, right: Tree[A]): Branch[A] = Branch(Leaf(left), right)
}

object Tree {

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => max(left).max(max(right))
  }
}