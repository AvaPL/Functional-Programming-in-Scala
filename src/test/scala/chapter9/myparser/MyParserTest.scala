package chapter9.myparser

import chapter9.ParseError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MyParserTest extends AnyWordSpec with Matchers {

  "string" when {
    "given a string" should {
      "parse it when it exactly matches parser" in {
        val string = "test"
        val parser = MyParser.string(string)

        val result = parser.run(string)

        result should be(Right(string))
      }

      "parse it when only prefix matches" in {
        val string = "test"
        val parser = MyParser.string(string)
        val input = string + "other"

        val result = parser.run(input)

        result should be(Right(string))
      }

      "return an error when it doesn't start with parser string" in {
        val string = "test"
        val parser = MyParser.string(string)
        val input = "other" + string

        val result = parser.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }

  "char" when {
    "given a char" should {
      "parse it" in {
        val char = 'a'
        val parser = MyParser.char(char)
        val input = char.toString

        val result = parser.run(input)

        result should be(Right(char))
      }
    }

    "given a word that doesn't start with that char" should {
      "return ParseError" in {
        val parser = MyParser.char('a')
        val input = "test"

        val result = parser.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }

  "regex" when {
    "given a regex" should {
      "parse the string if it matches regex" in {
        val parser = MyParser.regex("t..t".r)
        val input = "test"

        val result = parser.run(input)

        result should be(Right(input))
      }

      "return an error when regex doesn't match" in {
        val parser = MyParser.regex("fail".r)
        val input = "test"

        val result = parser.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }

  "succeed" when {
    "given any string" should {
      "return the given result" in {
        val parser = MyParser.succeed(5)

        val result = parser.run("any string")

        result should be(Right(5))
      }
    }
  }

  "or" should {
    "parse a string" when {
      "both parsers can parse it" in {
        val string = "test"
        val parser1 = MyParser.string(string)
        val parser2 = MyParser.string(string)
        val orParser = parser1.or(parser2)

        val result = orParser.run(string)

        result should be(Right(string))
      }

      "first parser can parse it" in {
        val string = "test"
        val parser1 = MyParser.string(string)
        val parser2 = MyParser.string("cannot parse")
        val orParser = parser1.or(parser2)

        val result = orParser.run(string)

        result should be(Right(string))
      }

      "second parser can parse it" in {
        val string = "test"
        val parser1 = MyParser.string("cannot parse")
        val parser2 = MyParser.string(string)
        val orParser = parser1.or(parser2)

        val result = orParser.run(string)

        result should be(Right(string))
      }
    }

    "return ParseError" when {
      "no parser can parse given string" in {
        val parser1 = MyParser.string("cannot parse")
        val parser2 = MyParser.string("also cannot parse")
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
        val parser1 = MyParser.char(char)
        val parser2 = MyParser.string('b'.toString)
        val orParser = parser1.either(parser2)

        val result = orParser.run(char.toString).toOption.get

        result should be(Left(char))
      }

      "both parsers can parse" in {
        val char = 'a'
        val parser1 = MyParser.char(char)
        val parser2 = MyParser.string(char.toString)
        val orParser = parser1.either(parser2)

        val result = orParser.run(char.toString).toOption.get

        result should be(Left(char))
      }
    }

    "parse the type on the right" when {
      "only right parser can parse" in {
        val string = "abc"
        val parser1 = MyParser.char('b')
        val parser2 = MyParser.string(string)
        val orParser = parser1.either(parser2)

        val result = orParser.run(string).toOption.get

        result should be(Right(string))
      }
    }
  }

  "listOfN" should {
    "parse a string" when {
      "number of occurrences matches the given number" in {
        val string = "test"
        val parser = MyParser.string(string)
        val parser3 = parser.listOfN(3)
        val string3 = string * 3

        val result = parser3.run(string3)

        result should be(Right(List.fill(3)(string)))
      }

      "parser is a compound parser" in {
        val char1 = 'a'
        val char2 = 'b'
        val string6 = char1.toString * 3 + char2.toString * 2 + char1.toString
        val parser1 = MyParser.char(char1)
        val parser2 = MyParser.char(char2)
        val parserOr = parser1.or(parser2)
        val parser6 = parserOr.listOfN(6)

        val result = parser6.run(string6)

        result should be(Right("aaabba".toCharArray.toList))
      }
    }

    "return ParseError" when {
      "number of occurrences is smaller than the given number" in {
        val string = "test"
        val parser = MyParser.string(string)
        val parser3 = parser.listOfN(3)
        val string2 = string * 2

        val result = parser3.run(string2)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "number of occurrences is smaller than the given number for compound parser" in {
        val string = "test"
        val parser1 = MyParser.string(string)
        val parser2 = MyParser.string("other")
        val parserOr = parser1.or(parser2)
        val parser3 = parserOr.listOfN(3)

        val result = parser3.run(string)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "matched tokens are not consecutive" in {
        val string = "test"
        val parser = MyParser.string(string)
        val parser3 = parser.listOfN(3)
        val stringNonConsecutive = string * 2 + "other" + string

        val result = parser3.run(stringNonConsecutive)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }

  "many" when {
    "given a parser" should {
      "parse empty string" in {
        val string = "abc"
        val parser = MyParser.string(string).many

        val result = parser.run(string * 0)

        result should be(Right(Nil))
      }

      "parse single occurrence" in {
        val string = "abc"
        val parser = MyParser.string(string).many

        val result = parser.run(string)

        result should be(Right(List(string)))
      }

      "parse multiple occurrences" in {
        val string = "abc"
        val parser = MyParser.string(string).many

        val result = parser.run(string * 5)

        result should be(Right(List.fill(5)(string)))
      }

      "parse when the end doesn't match" in {
        val string = "abc"
        val parser = MyParser.string(string).many

        val result = parser.run(string * 5 + "end")

        result should be(Right(List.fill(5)(string)))
      }
    }
  }

  "atLeastOne" when {
    "given a parser" should {
      "return an error on empty string" in {
        val string = "abc"
        val parser = MyParser.string(string).atLeastOne

        val result = parser.run(string * 0)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "parse single occurrence" in {
        val string = "abc"
        val parser = MyParser.string(string).atLeastOne

        val result = parser.run(string)

        result should be(Right(List(string)))
      }

      "parse multiple occurrences" in {
        val string = "abc"
        val parser = MyParser.string(string).atLeastOne

        val result = parser.run(string * 5)

        result should be(Right(List.fill(5)(string)))
      }

      "parse when the end doesn't match" in {
        val string = "abc"
        val parser = MyParser.string(string).atLeastOne

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
        val parser1 = MyParser.string(string)
        val parser2 = MyParser.char(char)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = string + char

        val result = parserFollowedBy.run(input)

        result should be(Right(string, char))
      }

      "return an error when the order is not correct" in {
        val string = "abc"
        val char = 'd'
        val parser1 = MyParser.string(string)
        val parser2 = MyParser.char(char)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = s"$char$string"

        val result = parserFollowedBy.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "return an error when first parser fails" in {
        val char = 'a'
        val parser1 = MyParser.string("fail")
        val parser2 = MyParser.char(char)
        val parserFollowedBy = parser1.followedBy(parser2)
        val input = "test" + char

        val result = parserFollowedBy.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "return an error when second parser fails" in {
        val string = "abc"
        val parser1 = MyParser.string(string)
        val parser2 = MyParser.char('a')
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
        val parser = MyParser.string(string)

        val mapped = parser.map(_.length)
        val result = mapped.run(string)

        result should be(Right(string.length))
      }

      "return the same error as without mapping in case of failure" in {
        val parser = MyParser.string("fail")
        val input = "test"

        val result = parser.run(input)
        val mappedResult = parser.map(_.length).run(input)

        (result, mappedResult) should matchPattern {
          case (Left(error1), Left(error2)) if error1 == error2 =>
        }
      }
    }
  }

  "flatMap" should {
    "return an error" when {
      "first parser fails" in {
        val parser1 = MyParser.string("test")
        val parser2 = parser1.flatMap(s => MyParser.char(s.charAt(0)))

        val result = parser2.run("fail")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "second parser fails" in {
        val string = "test"
        val parser1 = MyParser.string(string)
        val parser2 = parser1.flatMap(s => MyParser.char(s.charAt(0)))

        val result = parser2.run("testx")

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }

    "return a parser from argument" when {
      "both parsers succeed" in {
        val string = "abc"
        val char = string.charAt(0)
        val parser1 = MyParser.string(string)
        val parser2 = parser1.flatMap(s => MyParser.char(s.charAt(0)))
        val input = s"$string$char"

        val result = parser2.run(input)

        result should be(Right(char))
      }
    }
  }

  "map2" should {
    "return an error" when {
      "left parser fails" in {
        val char = 'a'
        val parser1 = MyParser.string("fail")
        val parser2 = MyParser.char(char)
        val map2Parser = parser1.map2(parser2)(_ + _)

        val result = map2Parser.run(char.toString)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }

      "right parser fails" in {
        val string = "test"
        val parser1 = MyParser.string(string)
        val parser2 = MyParser.char('a')
        val map2Parser = parser1.map2(parser2)(_ + _)

        val result = map2Parser.run(string)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }

    "parse input" when {
      "both parsers succeed" in {
        val string = "abc"
        val char = 'd'
        val parser1 = MyParser.string(string)
        val parser2 = MyParser.char(char)
        val map2Parser = parser1.map2(parser2)(_ + _)
        val input = s"$string$char"

        val result = map2Parser.run(input)

        result should be(Right(input))
      }
    }
  }
}
