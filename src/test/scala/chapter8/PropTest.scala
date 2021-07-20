package chapter8

import chapter6.Rng.Deterministic
import chapter8.Prop.{Falsified, Passed}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PropTest extends AnyWordSpec with Matchers {
  "forAll for Gen" should {
    "return Passed" when {
      "property is fulfilled for each generated input value" in {
        val gen = Gen.int
        val rng = Deterministic(5)

        val result = Prop.forAll(gen)(_ => true).run(10, rng)

        result should be(Passed)
      }
    }

    "return Falsified" when {
      "property is falsified for each generated input value" in {
        val gen = Gen.int
        val rng = Deterministic(5)

        val result = Prop.forAll(gen)(_ => false).run(10, rng)

        result should matchPattern {
          case Falsified(_, 0) =>
        }
      }

      "property is falsified after 3 successes" in {
        val gen = Gen.int
        val rng = Deterministic(1, 2, 3, 100, 5)

        val result = Prop.forAll(gen)(_ != 100).run(10, rng)

        result should matchPattern {
          case Falsified(_, 3) =>
        }
      }
    }
  }

  "&&" should {
    "return Pass" when {
      "both props are passed" in {
        val gen = Gen.int
        val rng = Deterministic(1, 2, 3, 4, 5)
        val notEqual100Prop = Prop.forAll(gen)(_ != 100)
        val positiveProp = Prop.forAll(gen)(_ > 0)
        val resultProp = notEqual100Prop.&&(positiveProp)

        val result = resultProp.run(10, rng)

        result should be(Passed)
      }
    }

    "return Falsified" when {
      "left prop at some point is falsified" in {
        val gen = Gen.int
        val rng = Deterministic(1, 2, 3, 100, 5)
        val notEqual100Prop = Prop.forAll(gen)(_ != 100)
        val positiveProp = Prop.forAll(gen)(_ > 0)
        val resultProp = notEqual100Prop.&&(positiveProp)

        val result = resultProp.run(10, rng)

        result should matchPattern {
          case Falsified(_, 3) =>
        }
      }

      "right prop at some point is falsified" in {
        val gen = Gen.int
        val rng = Deterministic(1, 2, 3, 100, 5)
        val positiveProp = Prop.forAll(gen)(_ > 0)
        val notEqual100Prop = Prop.forAll(gen)(_ != 100)
        val resultProp = positiveProp.&&(notEqual100Prop)

        val result = resultProp.run(10, rng)

        result should matchPattern {
          case Falsified(_, 3) =>
        }
      }

      "both props are falsified at some point" in {
        val gen = Gen.int
        val rng = Deterministic(1, -2, 3, 100, 5)
        val positiveProp = Prop.forAll(gen)(_ > 0)
        val notEqual100Prop = Prop.forAll(gen)(_ != 100)
        val resultProp = positiveProp.&&(notEqual100Prop)

        val result = resultProp.run(10, rng)

        result should matchPattern {
          case Falsified(_, 1) =>
        }
      }
    }
  }

  "||" should {
    "return Pass" when {
      "both props are passed" in {
        val gen = Gen.int
        val rng = Deterministic(1, 2, 3, 4, 5)
        val notEqual100Prop = Prop.forAll(gen)(_ != 100)
        val positiveProp = Prop.forAll(gen)(_ > 0)
        val resultProp = notEqual100Prop.||(positiveProp)

        val result = resultProp.run(10, rng)

        result should be(Passed)
      }

      "left prop at some point is falsified" in {
        val gen = Gen.int
        val rng = Deterministic(1, 2, 3, 100, 5)
        val notEqual100Prop = Prop.forAll(gen)(_ != 100)
        val positiveProp = Prop.forAll(gen)(_ > 0)
        val resultProp = notEqual100Prop.||(positiveProp)

        val result = resultProp.run(10, rng)

        result should be(Passed)
      }

      "right prop at some point is falsified" in {
        val gen = Gen.int
        val rng = Deterministic(1, 2, 3, 100, 5)
        val positiveProp = Prop.forAll(gen)(_ > 0)
        val notEqual100Prop = Prop.forAll(gen)(_ != 100)
        val resultProp = positiveProp.||(notEqual100Prop)

        val result = resultProp.run(10, rng)

        result should be(Passed)
      }
    }

    "return Falsified" when {
      "both props are falsified at some point" in {
        val gen = Gen.int
        val rng = Deterministic(1, -2, 3, 100, 5)
        val positiveProp = Prop.forAll(gen)(_ > 0)
        val notEqual100Prop = Prop.forAll(gen)(_ != 100)
        val resultProp = positiveProp.||(notEqual100Prop)

        val result = resultProp.run(10, rng)

        result should matchPattern {
          case _: Falsified =>
        }
      }
    }
  }

  "calculateSizes" when {
    "given a negative max size" should {
      "return empty List" in {
        Prop.calculateSizes(-5) should be(Nil)
      }
    }

    "given 0 max size" should {
      "return List(0)" in {
        Prop.calculateSizes(0) should be(List(0))
      }
    }

    "given power of 2 max size" should {
      "return 0 appended by incrementing power of 2 elements" in {
        Prop.calculateSizes(32) should be(List(0, 1, 2, 4, 8, 16, 32))
      }

      "return a List ending with that size" in {
        Prop.calculateSizes(1024).last should be(1024)
      }
    }

    "given max size that is not power of 2" should {
      "return a List ending with last power of 2 smaller than max size" in {
        Prop.calculateSizes(1023).last should be(512)
      }
    }
  }

  "calculateTestCases" when {
    "sizesCount is negative" should {
      "return empty List" in {
        Prop.calculateTestCases(1000, -5) should be(Nil)
      }
    }

    "sizesCount is 0" should {
      "return empty List" in {
        Prop.calculateTestCases(1000, -5) should be(Nil)
      }
    }

    "sizesCount is 1" should {
      "return a List with one element equal to testCases" in {
        Prop.calculateTestCases(123, 1) should be(List(123))
      }
    }

    "sizesCount is equal testCases" should {
      "return a List with only ones" in {
        Prop.calculateTestCases(5, 5) should be(List(1, 1, 1, 1, 1))
      }
    }

    "sizesCount is smaller than testCases" should {
      "return a List with ones padded by zeroes" in {
        Prop.calculateTestCases(3, 5) should be(List(1, 1, 1, 0, 0))
      }
    }

    "sizesCount is greater than testCases" should {
      "return linearly incrementing test cases" in {
        Prop.calculateTestCases(15, 5) should be(List(1, 2, 3, 4, 5))
      }

      "return list that sums to testCases and is sorted" in {
        val testCases = 1234567
        val result = Prop.calculateTestCases(testCases, 123)

        result shouldBe sorted
        result.sum should be(testCases)
      }
    }
  }

  "forAll for SGen" should {
    "return Passed" when {
      "property is fulfilled for each generated input value" in {
        val sgen = SGen.listOf(Gen.int)
        val rng = Deterministic(5)

        val result = Prop.forAll(sgen)(_ => true).run(10, 100, rng)

        result should be(Passed)
      }
    }

    "return Falsified" when {
      "property is falsified for each generated input value" in {
        val sgen = SGen.listOf(Gen.int)
        val rng = Deterministic(5)

        val result = Prop.forAll(sgen)(_ => false).run(10, 100, rng)

        result shouldBe a[Falsified]
      }

      "property is falsified for lists of size greater than 10" in {
        val sgen = SGen.listOf(Gen.int)
        val rng = Deterministic(1, 2, 3, 100, 5)

        // Will fail for lists of size 16, 32, 64
        val result = Prop.forAll(sgen)(_.size < 10).run(100, 100, rng)

        result shouldBe a[Falsified]
      }
    }
  }
}
