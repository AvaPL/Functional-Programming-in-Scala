package chapter6

import chapter6.Rng._
import org.scalamock.scalatest.MockFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RngTest extends AnyWordSpec with Matchers with MockFactory {
  "nonNegativeInt" when {
    "called" should {
      "return non negative random value multiple times" in {
        checkMultipleTimes(nonNegativeInt, (value: Int, _) => value should be >= 0)
      }

      "return non negative value when Int.MinValue is returned from RNG" in {
        val mockRng = mock[Rng]
        (mockRng.nextInt _).expects().returning((Int.MinValue, mockRng))

        nonNegativeInt(mockRng)._1 should be >= 0
      }
    }
  }

  "double" when {
    "called" should {
      "return double between 0 and 1 multiple times" in {
        checkMultipleTimes(double, (value: Double, _) => value should (be >= 0.0 and be < 1.0))
      }
    }
  }

  "ints" when {
    "given negative count" should {
      "return empty list" in {
        ints(-5)(Simple(0))._1 should be(Nil)
      }
    }

    "given 0 count" should {
      "return empty list" in {
        ints(0)(Simple(0))._1 should be(Nil)
      }
    }

    "given positive count" should {
      "return list of 1 random int" in {
        val rng = Simple(0)

        val (result, rng2) = ints(1)(rng)

        result.length should be(1)
        rng2 should not be rng
      }

      "return list of 10 random ints" in {
        val rng = Simple(0)

        val (result, rng2) = ints(10)(rng)

        result.length should be(10)
        rng2 should not be rng
      }

      "return initial RNG if no elements are generated" in {
        val rng = Simple(0)

        val (_, resultRng) = ints(0)(rng)

        resultRng should be(rng)
      }

      "return ints from RNG in correct order" in {
        val rng = Simple(0)
        val (expected1, expectedRng1) = rng.nextInt
        val (expected2, expectedRng2) = expectedRng1.nextInt

        val (result, resultRng) = ints(2)(rng)

        result should be(List(expected1, expected2))
        resultRng should be(expectedRng2)
      }
    }
  }

  "map2" when {
    "called" should {
      "return result of mapping two random values" in {
        val rng = Simple(0)
        val (expectedA, rngA) = rng.nextInt
        val (expectedB, rngB) = double(rngA)

        val (result, resultRng) = map2(_.nextInt, double)((a, b) => s"$a, $b")(rng)

        result should be(s"$expectedA, $expectedB")
        resultRng should be(rngB)
      }
    }
  }

  "sequence" when {
    val nextInt = (rng: Rng) => rng.nextInt

    "given empty list" should {
      "return Rand that returns an empty list" in {
        val rng = Simple(0)

        sequence(Nil)(rng) should be((Nil, rng))
      }
    }

    "given list with one element" should {
      "return rand with one element in list" in {
        val rng = Simple(0)
        val (expectedValue, expectedRng) = rng.nextInt

        val (result, resultRng) = sequence(List(nextInt))(rng)

        result should be(List(expectedValue))
        resultRng should be(expectedRng)
      }
    }

    "given list with multiple elements" should {
      "return elements in list in correct order" in {
        val rng = Simple(0)
        val (expectedValue1, expectedRng1) = rng.nextInt
        val (expectedValue2, expectedRng2) = expectedRng1.nextInt
        val (expectedValue3, expectedRng3) = expectedRng2.nextInt

        val (result, resultRng) = sequence(List(nextInt, nextInt, nextInt))(rng)

        result should be(List(expectedValue1, expectedValue2, expectedValue3))
        resultRng should be(expectedRng3)
      }
    }
  }

  "intsViaSequence" when {
    "called" should {
      "give same results as ints" in {
        val rng = Simple(0)

        intsViaSequence(1000)(rng) should be(ints(1000)(rng))
      }
    }
  }

  private def checkMultipleTimes[A](f: Rand[A], assertion: (A, Rng) => Assertion, rng: Rng = Simple(0), count: Int = 1000): Unit = {
    var currentRng = rng
    for (_ <- 0 until count) {
      val result = f(currentRng)
      assertion.tupled(result)
      currentRng = result._2
    }
  }
}
