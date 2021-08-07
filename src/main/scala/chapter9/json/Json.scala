package chapter9.json

import chapter9.{Parser, ParserGenerators}

trait Json

object Json {
  case object JNull extends Json

  case class JNumber(get: Double) extends Json

  case class JString(get: String) extends Json

  case class JBool(get: Boolean) extends Json

  case class JArray(get: IndexedSeq[Json]) extends Json

  case class JObject(get: Map[JString, Json]) extends Json

  def parser(generators: ParserGenerators): Parser[Json] = {
    def array: Parser[JArray] =
      generators.string("[")
        .followedBy(
          elements
            .or(whitespaces.map(_ => IndexedSeq.empty))
        )
        .followedBy(generators.string("]"))
        .flatten.map(_._2).map(JArray)

    def bool: Parser[JBool] =
      generators.string("true")
        .or(generators.string("false"))
        .map(_.toBoolean).map(JBool)

    def character: Parser[String] =
      generators.regex("""[^"\\]""".r)
        .or(
          generators.string("\\")
            .followedBy(escape)
            .string
        )

    def characters: Parser[String] =
      character
        .followedBy(characters)
        .string
        .or(generators.string(""))

    def digit: Parser[String] =
      generators.string("0")
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
        .followedBy(generators.string(","))
        .followedBy(elements)
        .flatten.map { case (element, _, elements) => element +: elements }
        .or(element.map(IndexedSeq(_)))

    def escape: Parser[String] =
      generators.regex("""["\\/bfnrtu]""".r)
        .or(
          generators.string("u")
            .followedBy(hex.listOfN(4))
            .map { case (u, hexes) => s"$u${hexes.mkString}" }
        )

    def exponent: Parser[String] =
      generators.regex("[eE]".r)
        .followedBy(sign)
        .followedBy(digits)
        .flatten.string
        .or(generators.string(""))

    def fraction: Parser[String] =
      generators.string(".")
        .followedBy(digits)
        .string
        .or(generators.string(""))

    def hex: Parser[String] =
      digit
        .or(generators.regex("[a-fA-F]".r))

    def integer: Parser[String] =
      onenine
        .followedBy(digits)
        .string
        .or(
          generators.string("-")
            .followedBy(digit)
            .string
        )
        .or(
          generators.string("-")
            .followedBy(onenine)
            .followedBy(digits)
            .flatten.string
        ).or(digit)

    def member: Parser[(JString, Json)] =
      whitespaces
        .followedBy(string)
        .followedBy(whitespaces)
        .followedBy(generators.string(":"))
        .followedBy(element)
        .map { case ((((_, string), _), _), element) => (string, element) }

    def members: Parser[Map[JString, Json]] =
      member
        .followedBy(generators.string(","))
        .followedBy(members)
        .flatten.map { case (member, _, members) => members + member }
        .or(member.map(Map(_)))

    def `null`: Parser[JNull.type] =
      generators.string("null").map(_ => JNull)

    def number: Parser[JNumber] =
      integer
        .followedBy(fraction)
        .followedBy(exponent)
        .flatten.string.map(_.toDouble).map(JNumber)

    def `object`: Parser[JObject] =
      generators.string("{")
        .followedBy(
          members
            .or(whitespaces.map(_ => Map[JString, Json]()))
        )
        .followedBy(generators.string("}"))
        .flatten.map(_._2).map(JObject)

    def onenine: Parser[String] =
      generators.regex("[1-9]".r)

    def sign: Parser[String] =
      generators.string("+")
        .or(generators.string("-"))
        .or(generators.string(""))

    def string: Parser[JString] =
      generators.string("\"")
        .followedBy(characters)
        .followedBy(generators.string("\""))
        .flatten.map { case (_, string, _) => string }.map(JString)

    def value: Parser[Json] =
      `object`
        .or(array)
        .or(string)
        .or(number)
        .or(bool)
        .or(`null`)

    def whitespaces: Parser[String] =
      generators.regex("\\s*".r)

    element
  }

  implicit class StringStringToString(val parser: Parser[(String, String)]) extends AnyVal {
    def string: Parser[String] = parser.map { case (s1, s2) => s"$s1$s2" }
  }

  implicit class StringStringStringToString(val parser: Parser[(String, String, String)]) extends AnyVal {
    def string: Parser[String] = parser.map { case (s1, s2, s3) => s"$s1$s2$s3" }
  }
}
