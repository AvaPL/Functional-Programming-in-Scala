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
    val whitespace: Parser[Nothing] = ???
    val value: Parser[Nothing] = ???
    val element = whitespace.followedBy(value).followedBy(whitespace).flatten
  }
}
