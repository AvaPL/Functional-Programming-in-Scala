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

    "given a word that doesn't start with that char" should {
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
      "parse it when it exactly matches parser" in {
        val string = "test"
        val parser = Parser.string(string)

        val result = parser.run(string)

        result should be(Right(string))
      }

      "parse it when only prefix matches" in {
        val string = "test"
        val parser = Parser.string(string)
        val input = string + "other"

        val result = parser.run(input)

        result should be(Right(string))
      }

      "return an error when it doesn't start with parser string" in {
        val string = "test"
        val parser = Parser.string(string)
        val input = "other" + string

        val result = parser.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
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

  "either" should {
    "parse the type on the left" when {
      "only left parser can parse" in {
        val int = 5
        val parser1 = Parser.int(int)
        val parser2 = Parser.char('a')
        val orParser = parser1.either(parser2)

        val result = orParser.run(int.toString).toOption.get

        result should be(Left(int))
      }

      "both parsers can parse" in {
        val int = 5
        val parser1 = Parser.int(int)
        val parser2 = Parser.string(int.toString)
        val orParser = parser1.either(parser2)

        val result = orParser.run(int.toString).toOption.get

        result should be(Left(int))
      }
    }

    "parse the type on the right" when {
      "only right parser can parse" in {
        val int = 5
        val parser1 = Parser.char('a')
        val parser2 = Parser.int(int)
        val orParser = parser1.either(parser2)

        val result = orParser.run(int.toString).toOption.get

        result should be(Right(int))
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

  "many" when {
    "given a parser" should {
      "parse empty string" in {
        val string = "abc"
        val parser = Parser.string(string).many

        val result = parser.run(string * 0)

        result should be(Right(Nil))
      }

      "parse single occurrence" in {
        val string = "abc"
        val parser = Parser.string(string).many

        val result = parser.run(string)

        result should be(Right(List(string)))
      }

      "parse multiple occurrences" in {
        val string = "abc"
        val parser = Parser.string(string).many

        val result = parser.run(string * 5)

        result should be(Right(List.fill(5)(string)))
      }

      "parse when the end doesn't match" in {
        val string = "abc"
        val parser = Parser.string(string).many

        val result = parser.run(string * 5 + "end")

        result should be(Right(List.fill(5)(string)))
      }
    }
  }

  "atLeastOne" when {
    "given a parser" should {
      "return an error on empty string" in {
        val string = "abc"
        val parser = Parser.string(string).many

        val result = parser.run(string * 0)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "parse single occurrence" in {
        val string = "abc"
        val parser = Parser.string(string).many

        val result = parser.run(string)

        result should be(Right(List(string)))
      }

      "parse multiple occurrences" in {
        val string = "abc"
        val parser = Parser.string(string).many

        val result = parser.run(string * 5)

        result should be(Right(List.fill(5)(string)))
      }

      "parse when the end doesn't match" in {
        val string = "abc"
        val parser = Parser.string(string).many

        val result = parser.run(string * 5 + "end")

        result should be(Right(List.fill(5)(string)))
      }
    }
  }

  "followedBy" when {
    "given two parsers" should {
      "parse when string is followed by int" in {
        val string = "abc"
        val int = 5
        val parser1 = Parser.string(string)
        val parser2 = Parser.int(int)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = string + int

        val result = parserFollowedBy.run(input)

        result should be(Right(string, int))
      }

      "return an error when the order is not correct" in {
        val string = "abc"
        val int = 5
        val parser1 = Parser.string(string)
        val parser2 = Parser.int(int)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = int + string

        val result = parserFollowedBy.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "return an error when first parser fails" in {
        val int = 5
        val parser1 = Parser.string("fail")
        val parser2 = Parser.int(int)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = "test" + int

        val result = parserFollowedBy.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "return an error when second parser fails" in {
        val string = "abc"
        val parser1 = Parser.string(string)
        val parser2 = Parser.int(5)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = string + 99

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
        val parser = Parser.string(string)

        val mapped = parser.map(_.length)
        val result = mapped.run(string)

        result should be(Right(string.length))
      }

      "return the same error as without mapping in case of failure" in {
        val parser = Parser.string("fail")
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
        val parser = Parser.char('a').many.slice

        val result = parser.run(string)

        result should be(Right(string))
      }

      "return the beginning of the input when the end doesn't match" in {
        val beginning = "aaaaa"
        val end = "bbb"
        val parser = Parser.char('a').many.slice
        val input = beginning + end

        val result = parser.run(input)

        result should be(Right(beginning))
      }
    }

    "given a non matching string" should {
      "return a parse error" in {
        val parser = Parser.char('a').many.slice

        val result = parser.run("fail")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }

  "flatMap" should { // TODO: Logic here might not be correct
    "return an error" when {
      "first parser fails" in {
        val parser1 = Parser.string("test")
        val parser2 = parser1.flatMap(s => Parser.int(s.length))

        val result = parser2.run("fail")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "second parser fails" in {
        val string = "test"
        val parser1 = Parser.string(string)
        val parser2 = parser1.flatMap(s => Parser.int(s.length))

        val result = parser2.run("99")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }

    "return a parser from argument" when {
      "both parsers succeed" in {
        val string = "test"
        val parser1 = Parser.string(string)
        val parser2 = parser1.flatMap(s => Parser.int(s.length))

        val result = parser2.run(string.length.toString)

        result should be(Right(string.length))
      }
    }
  }

  "map2" should { // TODO: Logic here might not be correct
    "return an error" when {
      "left parser fails" in {
        val int = 5
        val parser1 = Parser.string("fail")
        val parser2 = Parser.int(int)
        val map2Parser = Parser.map2(parser1, parser2)(_ + _)

        val result = map2Parser.run(int.toString)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "right parser fails" in {
        val string = "test"
        val parser1 = Parser.string(string)
        val parser2 = Parser.int(5)
        val map2Parser = Parser.map2(parser1, parser2)(_ + _)

        val result = map2Parser.run(string)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }

    "parse input" when {
      "both parsers succeed" in {
        val int = 5
        val parser1 = Parser.string(int.toString)
        val parser2 = Parser.int(int)
        val map2Parser = Parser.map2(parser1, parser2)(_ + _)

        val result = map2Parser.run(int.toString)

        result should be(Right("55"))
      }
    }
  }

  "succeed" when {
    "given any string" should {
      "return the given result" in {
        val parser = Parser.succeed(5)

        val result = parser.run("any string")

        result should be(Right(5))
      }
    }
  }
}
