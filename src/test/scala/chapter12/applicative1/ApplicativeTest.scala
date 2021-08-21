package chapter12.applicative1

import chapter12.validation.{Failure, Success, Validation}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ApplicativeTest extends AnyWordSpec with Matchers {
  "validation" when {
    "map2 is used on two Successes" should {
      "return a combination of values" in {
        val success1 = Success("a")
        val success2 = Success("b")

        val result = Applicative.validation.map2(success1, success2)(_ + _)

        result should be(Success("ab"))
      }
    }

    "map2 is used on Failure and Success" should {
      "return the Failure from left" in {
        val failure: Validation[String, String] = Failure("failure")
        val success = Success("test")

        val result = Applicative.validation.map2(failure, success)(_ + _)

        result should be(failure)
      }

      "return the Failure from right" in {
        val success = Success("test")
        val failure = Failure("failure")

        val result = Applicative.validation.map2(success, failure)(_ + _)

        result should be(failure)
      }
    }

    "map2 is used on two Failures" should {
      "combine the errors from failures" in {
        val failure1: Validation[String, String] = Failure("a", "b", "c")
        val failure2 = Failure("d", "e")

        val result = Applicative.validation.map2(failure1, failure2)(_ + _)

        val expected = Failure("a", "b", "c", "d", "e")
        result should be(expected)
      }
    }
  }
}
