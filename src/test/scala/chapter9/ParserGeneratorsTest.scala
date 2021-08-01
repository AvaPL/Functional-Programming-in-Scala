package chapter9

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserGeneratorsTest extends AnyWordSpec with Matchers with MockFactory {

  // TODO: Should be removed, tests should be conducted on concrete implementation
  def mockParserGenerators: ParserGenerators = mock[ParserGenerators]

  "string" when {
    "given a string" should {
      "parse it when it exactly matches parser" in {
        val string = "test"
        val parser = mockParserGenerators.string(string)

        val result = parser.run(string)

        result should be(Right(string))
      }

      "parse it when only prefix matches" in {
        val string = "test"
        val parser = mockParserGenerators.string(string)
        val input = string + "other"

        val result = parser.run(input)

        result should be(Right(string))
      }

      "return an error when it doesn't start with parser string" in {
        val string = "test"
        val parser = mockParserGenerators.string(string)
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
        val parser = mockParserGenerators.char(char)
        val input = char.toString

        val result = parser.run(input)

        result should be(Right(char))
      }
    }

    "given a word that doesn't start with that char" should {
      "return ParseError" in {
        val parser = mockParserGenerators.char('a')
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
        val parser = mockParserGenerators.regex("t..t".r)
        val input = "test"

        val result = parser.run(input)

        result should be(Right(input))
      }

      "return an error when regex doesn't match" in {
        val parser = mockParserGenerators.regex("fail".r)
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
        val parser = mockParserGenerators.succeed(5)

        val result = parser.run("any string")

        result should be(Right(5))
      }
    }
  }
}
