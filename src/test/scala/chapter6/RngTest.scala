package chapter6

import chapter6.Rng._
import org.scalamock.scalatest.MockFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RngTest extends AnyWordSpec with Matchers with MockFactory {
  "nonNegativeInt" should {
    "return non negative random value multiple times" in {
      checkMultipleTimes(nonNegativeInt, (value: Int, _) => value should be >= 0)
    }

    "return non negative value when Int.MinValue is returned from RNG" in {
      val mockRng = mock[Rng]
      (() => mockRng.nextInt).expects().returning((Int.MinValue, mockRng))

      nonNegativeInt(mockRng)._1 should be >= 0
    }
  }

  "double" should {
    "return double between 0 and 1 multiple times" in {
      checkMultipleTimes(double, (value: Double, _) => value should (be >= 0.0 and be < 1.0))
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

  "map2" should {
    "return result of mapping two random values" in {
      val rng = Simple(0)
      val (expectedA, rngA) = rng.nextInt
      val (expectedB, rngB) = double(rngA)

      val (result, resultRng) = map2(_.nextInt, double)((a, b) => s"$a, $b")(rng)

      result should be(s"$expectedA, $expectedB")
      resultRng should be(rngB)
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

  "intsViaSequence" should {
    "give same results as ints" in {
      val rng = Simple(0)

      intsViaSequence(1000)(rng) should be(ints(1000)(rng))
    }
  }

  "flatMap" should {
    "apply a function to result of first Rand" in {
      val rng = Simple(0)
      val rand = int
      val expected = int(rng) match {
        case (value, rng) => (value.toString, rng)
      }

      flatMap(rand)(int => unit(int.toString))(rng) should be(expected)
    }
  }

  "nonNegativeLessThan" should {
    "return non negative number less than given one multiple times" in {
      checkMultipleTimes(nonNegativeLessThan(20), (value: Int, _) => value should be < 20)
    }
  }

  "mapViaFlatMap" should {
    "give same result as map" in {
      val rng = Simple(0)

      mapViaFlatMap(int)(_.toString)(rng) should be(map(int)(_.toString)(rng))
    }
  }

  "map2ViaFlatMap" should {
    "give same result as map2" in {
      val rng = Simple(0)
      val intDoubleFunction = (a: Int, b: Double) => s"$a, $b"

      map2ViaFlatMap(int, double)(intDoubleFunction)(rng) should be(map2(int, double)(intDoubleFunction)(rng))
    }
  }

  "Deterministic" should {
    "return valid first value" when {
      "given a list of values" in {
        val rng = Deterministic(2, 4, 6)

        rng.nextInt._1 should be(2)
      }
    }

    "return same value" when {
      "given only one value" in {
        val rng = Deterministic(5)

        checkMultipleTimes(Rng.int, (value: Int, _) => value should be(5), rng)
      }
    }

    "alternate between two values" when {
      "given two values" in {
        val rng = Deterministic(5, 10)

        def assertion(value: Int, rng: Rng) = {
          val nextValue = value match {
            case 5 => 10
            case 10 => 5
          }
          rng.nextInt._1 should be(nextValue)
        }

        checkMultipleTimes(Rng.int, assertion, rng)
      }
    }

    "loop over list of values" when {
      "given a list of values" in {
        val rng = Deterministic(5, 10, 15, 20)

        def assertion(value: Int, rng: Rng) = {
          val nextValue = value match {
            case 5 => 10
            case 10 => 15
            case 15 => 20
            case 20 => 5
          }
          rng.nextInt._1 should be(nextValue)
        }

        checkMultipleTimes(Rng.int, assertion, rng)
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
