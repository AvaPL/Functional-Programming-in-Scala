package chapter8

case class SGen[A](forSize: Int => Gen[A]) {
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(size => forSize(size).flatMap(f(_).forSize(size)))
}

object SGen {
  def listOf[A](gen: Gen[A]): SGen[List[A]] =
    SGen(gen.listOfN)

  def nonEmptyListOf[A](gen: Gen[A]): SGen[List[A]] =
    SGen(i => gen.listOfN(i.max(1)))
}
