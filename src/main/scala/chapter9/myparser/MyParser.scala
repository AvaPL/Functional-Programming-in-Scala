package chapter9.myparser

import chapter9.{Failure, ParseError, Parser, ParserGenerators, Result, Success}

import scala.util.matching.Regex

case class MyParser[+A](parse: String => Result[A]) extends Parser[A] {
  override def generators: ParserGenerators = MyParser

  override def parseResult(input: String): Result[A] = parse(input)

  override def run(input: String): Either[ParseError, A] = parseResult(input) match {
    case Success(a, _) => Right(a)
    case Failure(error) => Left(error)
  }

  override def either[U](other: => Parser[U]): Parser[Either[A, U]] = MyParser { input =>
    this.parseResult(input) match {
      case Success(a, charsConsumed) => Success(Left(a), charsConsumed)
      case Failure(_) => other.parseResult(input) match {
        case Success(u, charsConsumed) => Success(Right(u), charsConsumed)
        case failure@Failure(_) => failure
      }
    }
  }

  override def flatMap[U](f: A => Parser[U]): Parser[U] = MyParser { input =>
    this.parseResult(input) match {
      case Success(a, aCharsConsumed) => f(a).parseResult(input.drop(aCharsConsumed)) match {
        case Success(u, uCharsConsumed) => Success(u, aCharsConsumed + uCharsConsumed)
        case failure@Failure(_) => failure
      }
      case failure@Failure(_) => failure
    }
  }
}

object MyParser extends ParserGenerators {
  override def string(string: String): Parser[String] = MyParser { input =>
    if (input.startsWith(string))
      Success(string, string.length)
    else
      Failure(ParseError(s"Expected '$string' but got '${input.take(string.length).mkString}'"))
  }

  override def regex(regex: Regex): Parser[String] = MyParser { input =>
    regex.findFirstIn(input) match {
      case Some(string) => Success(string, string.length)
      case None => Failure(ParseError(s"Expected '${regex.regex}' but got '$input'"))
    }
  }

  override def succeed[T](result: T): Parser[T] = MyParser(_ => Success(result, 0))
}
