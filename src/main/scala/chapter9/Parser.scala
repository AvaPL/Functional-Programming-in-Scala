package chapter9

trait Parser[T] {
  def run(input: String): Either[ParseError, T]

  def or(other: Parser[T]): Parser[T] = ???

  def either[U](other: Parser[U]): Parser[Either[T, U]] = ???

  def listOfN(n: Int): Parser[List[T]] = ???

  def many: Parser[List[T]] = ???

  def atLeastOne: Parser[List[T]] = ???

  def followedBy[U](other: Parser[U]): Parser[(T, U)] = ???

  def map[U](f: T => U): Parser[U] = ???
}

object Parser {
  def char(c: Char): Parser[Char] = ???

  def string(string: String): Parser[String] = ???

  def int(int: Int): Parser[Int] = ???

  def boolean(boolean: Boolean): Parser[Boolean] = ???
}
