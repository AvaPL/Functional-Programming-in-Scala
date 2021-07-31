package chapter9

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserGeneratorsTest extends AnyWordSpec with Matchers with MockFactory {

  "char" when {
    "given a char" should {
      "parse it" in {
        val char = 'a'
        val parser = parserGenerators.char(char)
        val input = char.toString

        val result = parser.run(input)

        result should be(Right(char))
      }
    }

    "given a word that doesn't start with that char" should {
      "return ParseError" in {
        val parser = parserGenerators.char('a')
        val input = "test"

        val result = parser.run(input)

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }

    def parserGenerators = {
      val generators = mock[ParserGenerators]
      (generators.string _).expects(*).onCall { s: String =>
        stringParser(firstChar = s.charAt(0))
      }
      generators
    }

    def stringParser(firstChar: Char) = {
      val parserString = mock[Parser[String]]
      (parserString.map[Char] _)
        .expects(*)
        .returning(charParser(firstChar))
      parserString
    }

    def charParser(char: Char) = {
      val parserChar = mock[Parser[Char]]
      (parserChar.run _)
        .expects(*)
        .onCall { s: String =>
          Either.cond(s.charAt(0) == char, char, mock[ParseError])
        }
      parserChar
    }
  }
}
