package chapter4

import chapter4.Chapter4._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chapter4Test extends AnyWordSpec with Matchers with Inside {

  "variance" when {
    "given empty list" should {
      "return None" in {
        variance(Seq()) should be(None)
      }
    }

    "given same numbers" should {
      "return 0" in {
        variance(Seq(1, 1, 1)) should be(Some(0))
      }
    }

    "given numbers with variance" should {
      "return 2.8 for 1, 1, 2, 3, 5" in {
        val result = variance(Seq(1, 1, 2, 3, 5))

        inside(result) {
          case Some(value) => value should be(2.24 +- 0.01)
        }
      }
    }
  }
}
