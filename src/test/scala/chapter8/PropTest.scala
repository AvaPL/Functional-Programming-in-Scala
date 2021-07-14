package chapter8

import chapter6.Rng.Deterministic
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

    "return Failure" when {
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

  // TODO: Tests for && and ||
}
