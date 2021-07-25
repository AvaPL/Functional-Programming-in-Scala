package chapter9

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserTest extends AnyWordSpec with Matchers {
  "char" when {
    "given a char" should {
      "parse it" in {
        val char = 'a'
        val parser = Parser.char(char)
        val input = char.toString

        val result = parser.run(input)

        result should be(Right(char))
      }
    }

    "given a word" should {
      "return ParseError" in {
        val parser = Parser.char('a')
        val input = "test"

        val result = parser.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }

  "string" when {
    "given a string" should {
      "parse it" in {
        val string = "test"
        val parser = Parser.string(string)

        val result = parser.run(string)

        result should be(Right(string))
      }
    }
  }

  "or" should {
    "parse a string" when {
      "both parsers can parse it" in {
        val string = "test"
        val parser1 = Parser.string(string)
        val parser2 = Parser.string(string)
        val orParser = parser1.or(parser2)

        val result = orParser.run(string)

        result should be(Right(string))
      }

      "first parser can parse it" in {
        val string = "test"
        val parser1 = Parser.string(string)
        val parser2 = Parser.string("cannot parse")
        val orParser = parser1.or(parser2)

        val result = orParser.run(string)

        result should be(Right(string))
      }

      "second parser can parse it" in {
        val string = "test"
        val parser1 = Parser.string("cannot parse")
        val parser2 = Parser.string(string)
        val orParser = parser1.or(parser2)

        val result = orParser.run(string)

        result should be(Right(string))
      }
    }

    "return ParseError" when {
      "no parser can parse given string" in {
        val parser1 = Parser.string("cannot parse")
        val parser2 = Parser.string("also cannot parse")
        val orParser = parser1.or(parser2)

        val result = orParser.run("test")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }
}
