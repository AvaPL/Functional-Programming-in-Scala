package chapter9.json

import chapter9.{Parser, Result}

trait Json

object Json extends Parser[Json] {
  case object JNull extends Json

  case class JNumber(get: Double) extends Json

  case class JString(get: String) extends Json

  case class JBool(get: Boolean) extends Json

  case class JArray(get: IndexedSeq[Json]) extends Json

  case class JObject(get: Map[JString, Json]) extends Json

  override def parse(input: String): Result[Json] = element.parse(input)

  def array: Parser[JArray] =
    Parser.string("[")
      .followedBy(
        elements
          .or(whitespaces.map(_ => IndexedSeq.empty))
      )
      .followedBy(Parser.string("]"))
      .flatten.map(_._2).map(JArray)

  def bool: Parser[JBool] =
    Parser.string("true")
      .or(Parser.string("false"))
      .map(_.toBoolean).map(JBool)

  def character: Parser[String] =
    Parser.regex("""[^"\\]""".r)
      .or(
        Parser.string("\\")
          .followedBy(escape)
          .string
      )

  def characters: Parser[String] =
    character
      .followedBy(characters)
      .string
      .or(Parser.string(""))

  def digit: Parser[String] =
    Parser.string("0")
      .or(onenine)

  def digits: Parser[String] =
    digit
      .followedBy(digits)
      .string
      .or(digit)

  def element: Parser[Json] =
    whitespaces
      .followedBy(value)
      .followedBy(whitespaces)
      .flatten.map(_._2)

  def elements: Parser[IndexedSeq[Json]] =
    element
      .followedBy(Parser.string(","))
      .followedBy(elements)
      .flatten.map { case (element, _, elements) => element +: elements }
      .or(element.map(IndexedSeq(_)))

  def escape: Parser[String] =
    Parser.regex("""["\\/bfnrtu]""".r)
      .or(
        Parser.string("u")
          .followedBy(hex.listOfN(4))
          .map { case (u, hexes) => s"$u${hexes.mkString}" }
      )

  def exponent: Parser[String] =
    Parser.regex("[eE]".r)
      .followedBy(sign)
      .followedBy(digits)
      .flatten.string
      .or(Parser.string(""))

  def fraction: Parser[String] =
    Parser.string(".")
      .followedBy(digits)
      .string
      .or(Parser.string(""))

  def hex: Parser[String] =
    digit
      .or(Parser.regex("[a-fA-F]".r))

  def integer: Parser[String] =
    onenine
      .followedBy(digits)
      .string
      .or(
        Parser.string("-")
          .followedBy(digit)
          .string
      )
      .or(
        Parser.string("-")
          .followedBy(onenine)
          .followedBy(digits)
          .flatten.string
      ).or(digit)

  def member: Parser[(JString, Json)] =
    whitespaces
      .followedBy(string)
      .followedBy(whitespaces)
      .followedBy(Parser.string(":"))
      .followedBy(element)
      .map { case ((((_, string), _), _), element) => (string, element) }

  def members: Parser[Map[JString, Json]] =
    member
      .followedBy(Parser.string(","))
      .followedBy(members)
      .flatten.map { case (member, _, members) => members + member }
      .or(member.map(Map(_)))

  def `null`: Parser[JNull.type] =
    Parser.string("null").map(_ => JNull)

  def number: Parser[JNumber] =
    integer
      .followedBy(fraction)
      .followedBy(exponent)
      .flatten.string.map(_.toDouble).map(JNumber)

  def `object`: Parser[JObject] =
    Parser.string("{")
      .followedBy(
        members
          .or(whitespaces.map(_ => Map[JString, Json]()))
      )
      .followedBy(Parser.string("}"))
      .flatten.map(_._2).map(JObject)

  def onenine: Parser[String] =
    Parser.regex("[1-9]".r)

  def sign: Parser[String] =
    Parser.string("+")
      .or(Parser.string("-"))
      .or(Parser.string(""))

  def string: Parser[JString] =
    Parser.string("\"")
      .followedBy(characters)
      .followedBy(Parser.string("\""))
      .flatten.map { case (_, string, _) => string }.map(JString)

  def value: Parser[Json] =
    `object`
      .or(array)
      .or(string)
      .or(number)
      .or(bool)
      .or(`null`)

  def whitespaces: Parser[String] =
    Parser.regex("\\s*".r)

  implicit class Flatten[T, U, V](val parser: Parser[((T, U), V)]) extends AnyVal {
    def flatten: Parser[(T, U, V)] = parser.map {
      case ((t, u), v) => (t, u, v)
    }
  }

  implicit class StringStringToString(val parser: Parser[(String, String)]) extends AnyVal {
    def string: Parser[String] = parser.map { case (s1, s2) => s"$s1$s2" }
  }

  implicit class StringStringStringToString(val parser: Parser[(String, String, String)]) extends AnyVal {
    def string: Parser[String] = parser.map { case (s1, s2, s3) => s"$s1$s2$s3" }
  }
}
