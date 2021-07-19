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
        // TODO: Fill
      }
    }

    "given 0 max size" should {
      "return List(0)" in {
        // TODO: Fill
      }
    }

    "given power of 2 max size" should {
      "return 0 appended by incrementing power of 2 elements" in {
        // TODO: Fill
      }

      "return a List ending with that size" in {
        // TODO: Fill
      }
    }

    "given max size that is not power of 2" should {
      "return a List ending with last power of 2 smaller than max size" in {
        // TODO: Fill
      }
    }
  }

  "calculateTestCases" when {
    // TODO: Fill
  }

  "linearTestCases" when {
    // TODO: Fill
  }
}
