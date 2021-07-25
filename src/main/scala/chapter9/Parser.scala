package chapter9

trait Parser[T] {
  def run(input: String): Either[ParseError, T]

  def or(other: Parser[T]): Parser[T] = ???
}

object Parser {
  def char(c: Char): Parser[Char] = ???

  def string(string: String): Parser[String] = ???
}
