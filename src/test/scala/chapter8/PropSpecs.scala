package chapter8

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

// Test won't fail even if not passed, see the console log instead
class PropSpecs extends AnyWordSpec with Matchers {
  "max" when {
    "used on a List" should {
      "return biggest value" in {
        val ints = Gen.choose(-100, 100)
        val maxProp = Prop.forAll(SGen.nonEmptyListOf(ints)) { list =>
          val max = list.max
          !list.exists(_ > max)
        }

        maxProp.check()
      }
    }
  }

  "sorted" when {
    "used on a List" should {
      "sort a List of Ints" in {
        val ints = Gen.choose(-100, 100)
        val sortedProp = Prop.forAll(SGen.listOf(ints)) { list =>
          val sorted = list.sorted
          sorted.sliding(2).foldLeft(true) {
            case (acc, List(a, b)) => acc && a <= b
            case (acc, _) => acc
          }
        }

        sortedProp.check()
      }
    }
  }
}
