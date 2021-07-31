package chapter9

import scala.util.matching.Regex

trait Parser[T] {
  def run(input: String): Either[ParseError, T]

  def or(other: => Parser[T]): Parser[T] = this.either(other).map {
    case Right(value) => value
    case Left(value) => value
  }

  def either[U](other: => Parser[U]): Parser[Either[T, U]] = ???

  def listOfN(n: Int): Parser[List[T]] =
    if (n <= 0)
      Parser.succeed(List.empty[T])
    else
      this.map2(this.listOfN(n - 1))(_ :: _)

  def many: Parser[List[T]] =
    this.map2(this.many)(_ :: _).or(Parser.succeed(Nil))

  def atLeastOne: Parser[List[T]] = this.followedBy(this.many).map {
    case (head, tail) => head :: tail
  }

  def followedBy[U](other: => Parser[U]): Parser[(T, U)] =
    this.flatMap(t => other.map((t, _)))

  def map[U](f: T => U): Parser[U] =
    this.flatMap(t => Parser.succeed(f(t)))

  def map2[U, V](parser2: => Parser[U])(f: (T, U) => V): Parser[V] =
    this.flatMap(t => parser2.map(f(t, _)))

  def flatMap[U](f: T => Parser[U]): Parser[U] = ???

  /**
   * Returns the part of the string that is examined when the
   * parser is successful.
   */
  def slice: Parser[String] = ???
}

object Parser {
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def string(string: String): Parser[String] = ???

  def int(int: Int): Parser[Int] =
    regex(int.toString.r).map(_.toInt)

  def regex(regex: Regex): Parser[String] = ???

  def succeed[T](result: T): Parser[T] = ???
}
