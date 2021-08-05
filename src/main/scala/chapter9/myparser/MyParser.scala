package chapter9.myparser

import chapter9.{ParseError, Parser, ParserGenerators}

import scala.util.matching.Regex

case class MyParser[+A]() extends Parser[A]{
  override def generators: ParserGenerators = MyParser

  override def run(input: String): Either[ParseError, A] = ???

  override def either[U](other: => Parser[U]): Parser[Either[A, U]] = ???

  override def flatMap[U](f: A => Parser[U]): Parser[U] = ???

  override def slice: Parser[String] = ???
}

object MyParser extends ParserGenerators {
  override def string(string: String): Parser[String] = ???

  override def regex(regex: Regex): Parser[String] = ???

  override def succeed[T](result: T): Parser[T] = ???
}
