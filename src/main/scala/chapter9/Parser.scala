package chapter9

import chapter9.Parser.map2

trait Parser[T] {
  def run(input: String): Either[ParseError, T]

  def or(other: Parser[T]): Parser[T] = ???

  def either[U](other: Parser[U]): Parser[Either[T, U]] = ???

  def listOfN(n: Int): Parser[List[T]] = ???

  def many: Parser[List[T]] = ???

  def atLeastOne: Parser[List[T]] = this.followedBy(this.many).map {
    case (head, tail) => head :: tail
  }

  def followedBy[U](other: Parser[U]): Parser[(T, U)] = ???

  def map[U](f: T => U): Parser[U] =
    map2(this, Parser.succeed(())) {
      case (t, _) => f(t)
    }

  def flatMap[U](f: T => Parser[U]): Parser[U] = ???

  /**
   * Returns the part of the string that is examined when the
   * parser is successful.
   */
  def slice: Parser[String] = ???
}

object Parser {
  def char(c: Char): Parser[Char] = ???

  def string(string: String): Parser[String] = ???

  def int(int: Int): Parser[Int] = ???

  def boolean(boolean: Boolean): Parser[Boolean] = ???

  def map2[T, U, V](parser1: Parser[T], parser2: Parser[U])(f: (T, U) => V): Parser[V] =
    parser1.flatMap(t => parser2.map(f(t, _)))

  def succeed[T](result: T): Parser[T] =
    string("").map(_ => result)
}
