package chapter9

import scala.util.matching.Regex

trait Parser[+T] {
  def parse(input: String): Result[T]

  def either[U](other: => Parser[U]): Parser[Either[T, U]] = input =>
    this.parse(input) match {
      case Success(a, charsConsumed) => Success(Left(a), charsConsumed)
      case Failure(_) => other.parse(input) match {
        case Success(u, charsConsumed) => Success(Right(u), charsConsumed)
        case failure@Failure(_) => failure
      }
    }

  def flatMap[U](f: T => Parser[U]): Parser[U] = input =>
    this.parse(input) match {
      case Success(a, aCharsConsumed) => f(a).parse(input.drop(aCharsConsumed)) match {
        case Success(u, uCharsConsumed) => Success(u, aCharsConsumed + uCharsConsumed)
        case failure@Failure(_) => failure
      }
      case failure@Failure(_) => failure
    }

  def or[U >: T](other: => Parser[U]): Parser[U] = this.either(other).map {
    case Left(value) => value
    case Right(value) => value
  }

  def listOfN(n: Int): Parser[List[T]] =
    if (n <= 0)
      Parser.succeed(Nil)
    else
      this.map2(this.listOfN(n - 1))(_ :: _)

  def map2[U, V](parser2: => Parser[U])(f: (T, U) => V): Parser[V] =
    this.flatMap(t => parser2.map(f(t, _)))

  def many: Parser[List[T]] =
    this.map2(this.many)(_ :: _).or(Parser.succeed(Nil))

  def atLeastOne: Parser[List[T]] = this.followedBy(this.many).map {
    case (head, tail) => head :: tail
  }

  def followedBy[U](other: => Parser[U]): Parser[(T, U)] =
    map2(other)((t, u) => (t, u))

  def map[U](f: T => U): Parser[U] =
    this.flatMap(t => Parser.succeed(f(t)))
}

object Parser {
  def string(string: String): Parser[String] = input =>
    if (input.startsWith(string))
      Success(string, string.length)
    else
      Failure(ParseError(s"Expected '$string' but got '${input.take(string.length).mkString}'"))

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def regex(regex: Regex): Parser[String] = input =>
    regex.findPrefixOf(input) match {
      case Some(string) => Success(string, string.length)
      case None => Failure(ParseError(s"Expected '${regex.regex}' but got '$input'"))
    }

  def succeed[T](result: T): Parser[T] = _ =>
    Success(result, 0)
}
