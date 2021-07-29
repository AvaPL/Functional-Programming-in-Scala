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

  "listOfN" should {
    "parse a string" when {
      "number of occurrences matches the given number" in {
        val string = "test"
        val parser = Parser.string(string)
        val parser3 = parser.listOfN(3)
        val string3 = string * 3

        val result = parser3.run(string3)

        result should be(Right(string3))
      }

      "parser is a compound parser" in {
        val char1 = 'a'
        val char2 = 'b'
        val string6 = char1.toString * 3 + char2.toString * 2 + char1.toString
        val parser1 = Parser.char(char1)
        val parser2 = Parser.char(char2)
        val parserOr = parser1.or(parser2)
        val parser6 = parserOr.listOfN(6)

        val result = parser6.run(string6)

        result should be(Right(string6))
      }
    }

    "return ParseError" when {
      "number of occurrences is smaller than the given number" in {
        val string = "test"
        val parser = Parser.string(string)
        val parser3 = parser.listOfN(3)
        val string6 = string * 6

        val result = parser3.run(string6)

        result should be(Right(string6))
      }

      "number of occurrences is greater than the given number" in {
        val string = "test"
        val parser = Parser.string(string)
        val parser3 = parser.listOfN(3)
        val string2 = string * 2

        val result = parser3.run(string2)

        result should be(Right(string2))
      }

      "matched tokens are not consecutive" in {
        val string = "test"
        val parser = Parser.string(string)
        val parser3 = parser.listOfN(3)
        val stringNonConsecutive = string * 2 + "other" + string

        val result = parser3.run(stringNonConsecutive)

        result should be(Right(stringNonConsecutive))
      }
    }
  }
}
