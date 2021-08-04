package chapter9.json

import chapter9.Parser

trait Json

object Json {
  case object JNull extends Json

  case class JNumber(get: Double) extends Json

  case class JString(get: String) extends Json

  case class JBool(get: Boolean) extends Json

  case class JArray(get: IndexedSeq[Json]) extends Json

  case class JObject(get: Map[String, Json]) extends Json

  def parser(generators: JsonParserGenerators): Parser[Json] = {
    val `object`: Parser[Nothing] = ???
    val array: Parser[Nothing] = ???
    val character: Parser[String] = generators.regex("""[^"\\]""".r)
    val characters: Parser[String] = generators.string("").or(character.followedBy(characters).string)
    val string: Parser[JString] = generators.string("\"").followedBy(characters).followedBy(generators.string("\"")).flatten.map(_.toString)
    val onenine: Parser[String] = generators.regex("[1-9]".r)
    val digit: Parser[String] = generators.string("0").or(onenine)
    val digits: Parser[String] = digit.or(digit.followedBy(digits).string)
    val integer: Parser[String] = digit.or(onenine.followedBy(digits).string).or(generators.string("-").followedBy(digit).string).or(generators.string("-").followedBy(onenine).followedBy(digits).flatten.string)
    val fraction: Parser[String] = generators.string("").or(generators.string(".").followedBy(digits).string)
    val sign: Parser[String] = generators.string("").or(generators.string("+")).or(generators.string("-"))
    val exponent: Parser[String] = generators.string("").or(generators.string("E").followedBy(sign).followedBy(digits).flatten.string)
    val number: Parser[JNumber] = integer.followedBy(fraction).followedBy(exponent).flatten.string.map(_.toDouble)
    val bool: Parser[JBool] = generators.string("true").or(generators.string("false")).map(_.toBoolean)
    val `null`: Parser[JNull.type] = generators.string("null").map(_ => JNull)
    val value: Parser[Json] = `object`.or(array).or(string).or(number).or(bool).or(`null`)
    val whitespaces: Parser[String] = generators.string("").or(generators.regex("\\s".r)).many.map(_.mkString)
    val element: Parser[Json] = whitespaces.followedBy(value).followedBy(whitespaces).flatten.map(_._2)
    val json: Parser[Json] = element
  }

  implicit class StringStringToString(val parser: Parser[(String, String)]) extends AnyVal {
    def string: Parser[String] = parser.map { case (s1, s2) => s"$s1$s2" }
  }

  implicit class StringStringStringToString(val parser: Parser[(String, String, String)]) extends AnyVal {
    def string: Parser[String] = parser.map { case (s1, s2, s3) => s"$s1$s2$s3" }
  }
}
