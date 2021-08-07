package chapter9

trait Parser[+T] {
  def generators: ParserGenerators

  def parseResult(input: String): Result[T]

  def either[U](other: => Parser[U]): Parser[Either[T, U]]

  def flatMap[U](f: T => Parser[U]): Parser[U]

  def run(input: String): Either[ParseError, T] = parseResult(input) match {
    case Success(a, _) => Right(a)
    case Failure(error) => Left(error)
  }

  def or[U >: T](other: => Parser[U]): Parser[U] = this.either(other).map {
    case Left(value) => value
    case Right(value) => value
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
    map2(other)((t, u) => (t, u))

  def map[U](f: T => U): Parser[U] =
    this.flatMap(t => generators.succeed(f(t)))

  def map2[U, V](parser2: => Parser[U])(f: (T, U) => V): Parser[V] =
    this.flatMap(t => parser2.map(f(t, _)))
}

sealed trait Result[+A]

case class Success[+A](a: A, charsConsumed: Int) extends Result[A]

case class Failure(error: ParseError) extends Result[Nothing]

object Parser {
  implicit class Flatten[T, U, V](val parser: Parser[((T, U), V)]) extends AnyVal {
    def flatten: Parser[(T, U, V)] = parser.map {
      case ((t, u), v) => (t, u, v)
    }
  }
}
