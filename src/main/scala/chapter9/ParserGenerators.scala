package chapter9

import scala.util.matching.Regex

trait ParserGenerators {
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def string(string: String): Parser[String]

  def regex(regex: Regex): Parser[String]

  def succeed[T](result: T): Parser[T]
}
