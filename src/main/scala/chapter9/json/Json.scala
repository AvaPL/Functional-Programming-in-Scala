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
          whitespaces.map(_ => IndexedSeq.empty)
            .or(elements)
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
      generators.string("")
        .or(
          character
            .followedBy(characters)
            .string
        )

    def digit: Parser[String] =
      generators.string("0")
        .or(onenine)

    def digits: Parser[String] =
      digit.or(
        digit
          .followedBy(digits)
          .string
      )

    def element: Parser[Json] =
      whitespaces
        .followedBy(value)
        .followedBy(whitespaces)
        .flatten.map(_._2)

    def elements: Parser[IndexedSeq[Json]] =
      element.map(IndexedSeq(_))
        .or(
          element
            .followedBy(generators.string(","))
            .followedBy(elements)
            .flatten.map { case (element, _, elements) => element +: elements }
        )

    def escape: Parser[String] =
      generators.regex("""["\\/bfnrtu]""".r)
        .or(
          generators.string("u")
            .followedBy(hex.listOfN(4))
            .map { case (u, hexes) => s"$u${hexes.mkString}" }
        )

    def exponent: Parser[String] =
      generators.string("")
        .or(
          generators.string("E")
            .followedBy(sign)
            .followedBy(digits)
            .flatten.string
        )

    def fraction: Parser[String] =
      generators.string("")
        .or(
          generators.string(".")
            .followedBy(digits)
            .string
        )

    def hex: Parser[String] =
      digit
        .or(generators.regex("[a-fA-F]".r))

    def integer: Parser[String] =
      digit
        .or(
          onenine
            .followedBy(digits)
            .string
        )
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
        )

    def member: Parser[(JString, Json)] =
      whitespaces
        .followedBy(string)
        .followedBy(whitespaces)
        .followedBy(generators.string(":"))
        .followedBy(element)
        .map { case ((((_, string), _), _), element) => (string, element) }

    def members: Parser[Map[JString, Json]] =
      member.map(Map(_))
        .or(
          member
            .followedBy(generators.string(","))
            .followedBy(members)
            .flatten.map { case (member, _, members) => members + member }
        )

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
          whitespaces.map(_ => Map[JString, Json]())
            .or(members)
        )
        .followedBy(generators.string("}"))
        .flatten.map(_._2).map(JObject)

    def onenine: Parser[String] =
      generators.regex("[1-9]".r)

    def sign: Parser[String] =
      generators.string("")
        .or(generators.string("+"))
        .or(generators.string("-"))

    def string: Parser[JString] =
      generators.string("\"")
        .followedBy(characters)
        .followedBy(generators.string("\""))
        .flatten.map(_.toString).map(JString)

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
