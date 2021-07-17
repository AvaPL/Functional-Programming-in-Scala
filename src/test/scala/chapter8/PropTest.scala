package chapter8

import chapter6.Rng.Deterministic
import chapter8.Prop.{Falsified, Passed}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PropTest extends AnyWordSpec with Matchers {
  "forAll" should {
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
}
