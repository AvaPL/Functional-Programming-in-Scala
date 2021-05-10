package chapter6

import chapter6.Rng.{Rand, Simple, double, ints, nonNegativeInt}
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
