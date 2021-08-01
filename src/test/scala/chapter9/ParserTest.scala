package chapter9

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserTest extends AnyWordSpec with Matchers with MockFactory {

  // TODO: Should be removed, tests should be conducted on concrete implementation
  def mockParserGenerators: ParserGenerators = mock[ParserGenerators]

  "or" should {
    "parse a string" when {
      "both parsers can parse it" in {
        val string = "test"
        val parser1 = mockParserGenerators.string(string)
        val parser2 = mockParserGenerators.string(string)
        val orParser = parser1.or(parser2)

        val result = orParser.run(string)

        result should be(Right(string))
      }

      "first parser can parse it" in {
        val string = "test"
        val parser1 = mockParserGenerators.string(string)
        val parser2 = mockParserGenerators.string("cannot parse")
        val orParser = parser1.or(parser2)

        val result = orParser.run(string)

        result should be(Right(string))
      }

      "second parser can parse it" in {
        val string = "test"
        val parser1 = mockParserGenerators.string("cannot parse")
        val parser2 = mockParserGenerators.string(string)
        val orParser = parser1.or(parser2)

        val result = orParser.run(string)

        result should be(Right(string))
      }
    }

    "return ParseError" when {
      "no parser can parse given string" in {
        val parser1 = mockParserGenerators.string("cannot parse")
        val parser2 = mockParserGenerators.string("also cannot parse")
        val orParser = parser1.or(parser2)

        val result = orParser.run("test")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }

  "either" should {
    "parse the type on the left" when {
      "only left parser can parse" in {
        val char = 'a'
        val parser1 = mockParserGenerators.char(char)
        val parser2 = mockParserGenerators.string('b'.toString)
        val orParser = parser1.either(parser2)

        val result = orParser.run(char.toString).toOption.get

        result should be(Left(char))
      }

      "both parsers can parse" in {
        val char = 'a'
        val parser1 = mockParserGenerators.char(char)
        val parser2 = mockParserGenerators.string(char.toString)
        val orParser = parser1.either(parser2)

        val result = orParser.run(char.toString).toOption.get

        result should be(Left(char))
      }
    }

    "parse the type on the right" when {
      "only right parser can parse" in {
        val char = 'a'
        val parser1 = mockParserGenerators.char('b')
        val parser2 = mockParserGenerators.string(char.toString)
        val orParser = parser1.either(parser2)

        val result = orParser.run(char.toString).toOption.get

        result should be(Right(char))
      }
    }
  }

  "listOfN" should {
    "parse a string" when {
      "number of occurrences matches the given number" in {
        val string = "test"
        val parser = mockParserGenerators.string(string)
        val parser3 = parser.listOfN(3)
        val string3 = string * 3

        val result = parser3.run(string3)

        result should be(Right(string3))
      }

      "parser is a compound parser" in {
        val char1 = 'a'
        val char2 = 'b'
        val string6 = char1.toString * 3 + char2.toString * 2 + char1.toString
        val parser1 = mockParserGenerators.char(char1)
        val parser2 = mockParserGenerators.char(char2)
        val parserOr = parser1.or(parser2)
        val parser6 = parserOr.listOfN(6)

        val result = parser6.run(string6)

        result should be(Right(string6))
      }
    }

    "return ParseError" when {
      "number of occurrences is smaller than the given number" in {
        val string = "test"
        val parser = mockParserGenerators.string(string)
        val parser3 = parser.listOfN(3)
        val string6 = string * 6

        val result = parser3.run(string6)

        result should be(Right(string6))
      }

      "number of occurrences is greater than the given number" in {
        val string = "test"
        val parser = mockParserGenerators.string(string)
        val parser3 = parser.listOfN(3)
        val string2 = string * 2

        val result = parser3.run(string2)

        result should be(Right(string2))
      }

      "matched tokens are not consecutive" in {
        val string = "test"
        val parser = mockParserGenerators.string(string)
        val parser3 = parser.listOfN(3)
        val stringNonConsecutive = string * 2 + "other" + string

        val result = parser3.run(stringNonConsecutive)

        result should be(Right(stringNonConsecutive))
      }
    }
  }

  "many" when {
    "given a parser" should {
      "parse empty string" in {
        val string = "abc"
        val parser = mockParserGenerators.string(string).many

        val result = parser.run(string * 0)

        result should be(Right(Nil))
      }

      "parse single occurrence" in {
        val string = "abc"
        val parser = mockParserGenerators.string(string).many

        val result = parser.run(string)

        result should be(Right(List(string)))
      }

      "parse multiple occurrences" in {
        val string = "abc"
        val parser = mockParserGenerators.string(string).many

        val result = parser.run(string * 5)

        result should be(Right(List.fill(5)(string)))
      }

      "parse when the end doesn't match" in {
        val string = "abc"
        val parser = mockParserGenerators.string(string).many

        val result = parser.run(string * 5 + "end")

        result should be(Right(List.fill(5)(string)))
      }
    }
  }

  "atLeastOne" when {
    "given a parser" should {
      "return an error on empty string" in {
        val string = "abc"
        val parser = mockParserGenerators.string(string).many

        val result = parser.run(string * 0)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "parse single occurrence" in {
        val string = "abc"
        val parser = mockParserGenerators.string(string).many

        val result = parser.run(string)

        result should be(Right(List(string)))
      }

      "parse multiple occurrences" in {
        val string = "abc"
        val parser = mockParserGenerators.string(string).many

        val result = parser.run(string * 5)

        result should be(Right(List.fill(5)(string)))
      }

      "parse when the end doesn't match" in {
        val string = "abc"
        val parser = mockParserGenerators.string(string).many

        val result = parser.run(string * 5 + "end")

        result should be(Right(List.fill(5)(string)))
      }
    }
  }

  "followedBy" when {
    "given two parsers" should {
      "parse when string is followed by char" in {
        val string = "abc"
        val char = 'd'
        val parser1 = mockParserGenerators.string(string)
        val parser2 = mockParserGenerators.char(char)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = string + char

        val result = parserFollowedBy.run(input)

        result should be(Right(string, char))
      }

      "return an error when the order is not correct" in {
        val string = "abc"
        val char = 'd'
        val parser1 = mockParserGenerators.string(string)
        val parser2 = mockParserGenerators.char(char)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = char + string

        val result = parserFollowedBy.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "return an error when first parser fails" in {
        val char = 'a'
        val parser1 = mockParserGenerators.string("fail")
        val parser2 = mockParserGenerators.char(char)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = "test" + char

        val result = parserFollowedBy.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "return an error when second parser fails" in {
        val string = "abc"
        val parser1 = mockParserGenerators.string(string)
        val parser2 = mockParserGenerators.char('a')
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = string + 'b'

        val result = parserFollowedBy.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }

  "map" when {
    "given a parser" should {
      "map over value with given function" in {
        val string = "abc"
        val parser = mockParserGenerators.string(string)

        val mapped = parser.map(_.length)
        val result = mapped.run(string)

        result should be(Right(string.length))
      }

      "return the same error as without mapping in case of failure" in {
        val parser = mockParserGenerators.string("fail")
        val input = "test"

        val result = parser.run(input)
        val mappedResult = parser.map(_.length).run(input)

        (result, mappedResult) should matchPattern {
          case (Left(error1), Left(error2)) if error1 == error2 =>
        }
      }
    }
  }

  "slice" when {
    "given a matching string" should {
      "return the whole input if it matches" in {
        val string = "aaaaa"
        val parser = mockParserGenerators.char('a').many.slice

        val result = parser.run(string)

        result should be(Right(string))
      }

      "return the beginning of the input when the end doesn't match" in {
        val beginning = "aaaaa"
        val end = "bbb"
        val parser = mockParserGenerators.char('a').many.slice
        val input = beginning + end

        val result = parser.run(input)

        result should be(Right(beginning))
      }
    }

    "given a non matching string" should {
      "return a parse error" in {
        val parser = mockParserGenerators.char('a').many.slice

        val result = parser.run("fail")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }

  "flatMap" should {
    // TODO: Logic here might not be correct
    "return an error" when {
      "first parser fails" in {
        val parser1 = mockParserGenerators.string("test")
        val parser2 = parser1.flatMap(s => mockParserGenerators.char(s.charAt(0)))

        val result = parser2.run("fail")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "second parser fails" in {
        val string = "test"
        val parser1 = mockParserGenerators.string(string)
        val parser2 = parser1.flatMap(s => mockParserGenerators.char(s.charAt(0)))

        val result = parser2.run("testx")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }

    "return a parser from argument" when {
      "both parsers succeed" in {
        val string = "a"
        val parser1 = mockParserGenerators.string(string)
        val parser2 = parser1.flatMap(s => mockParserGenerators.char(s.charAt(0)))

        val result = parser2.run(string)

        result should be(Right(string))
      }
    }
  }

  "map2" should {
    // TODO: Logic here might not be correct
    "return an error" when {
      "left parser fails" in {
        val char = 'a'
        val parser1 = mockParserGenerators.string("fail")
        val parser2 = mockParserGenerators.char(char)
        val map2Parser = parser1.map2(parser2)(_ + _)

        val result = map2Parser.run(char.toString)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "right parser fails" in {
        val string = "test"
        val parser1 = mockParserGenerators.string(string)
        val parser2 = mockParserGenerators.char('a')
        val map2Parser = parser1.map2(parser2)(_ + _)

        val result = map2Parser.run(string)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }

    "parse input" when {
      "both parsers succeed" in {
        val char = 'a'
        val parser1 = mockParserGenerators.string(char.toString)
        val parser2 = mockParserGenerators.char(char)
        val map2Parser = parser1.map2(parser2)(_ + _)

        val result = map2Parser.run(char.toString)

        result should be(Right("aa"))
      }
    }
  }
}
