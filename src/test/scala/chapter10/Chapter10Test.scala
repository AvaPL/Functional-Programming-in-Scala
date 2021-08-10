package chapter10

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chapter10Test extends AnyWordSpec with Matchers {
  "isSorted" when {
    "given a sorted sequence" should {
      "return true" in {
        val sorted = IndexedSeq(-4, 0, 0, 3, 3, 5)

        val result = Chapter10.isSorted(sorted)

        result should be(true)
      }
    }

    "given not sorted sequence" should {
      "return false" in {
        val notSorted = IndexedSeq(-4, 3, 3, 2, 5)

        val result = Chapter10.isSorted(notSorted)

        result should be(false)
      }
    }
  }
}
