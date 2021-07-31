package chapter9

trait Parser[T] {
  def generators: ParserGenerators

  def run(input: String): Either[ParseError, T]

  def either[U](other: => Parser[U]): Parser[Either[T, U]]

  def flatMap[U](f: T => Parser[U]): Parser[U]

  /**
   * Returns the part of the string that is examined when the
   * parser is successful.
   */
  def slice: Parser[String]

  def or(other: => Parser[T]): Parser[T] = this.either(other).map {
    case Right(value) => value
    case Left(value) => value
  }

  def listOfN(n: Int): Parser[List[T]] =
    if (n <= 0)
      generators.succeed(Nil)
    else
      this.map2(this.listOfN(n - 1))(_ :: _)

  def many: Parser[List[T]] =
    this.map2(this.many)(_ :: _).or(generators.succeed(Nil))

  def atLeastOne: Parser[List[T]] = this.followedBy(this.many).map {
    case (head, tail) => head :: tail
  }

  def followedBy[U](other: => Parser[U]): Parser[(T, U)] =
    this.flatMap(t => other.map((t, _)))

  def map[U](f: T => U): Parser[U] =
    this.flatMap(t => generators.succeed(f(t)))

  def map2[U, V](parser2: => Parser[U])(f: (T, U) => V): Parser[V] =
    this.flatMap(t => parser2.map(f(t, _)))
}
