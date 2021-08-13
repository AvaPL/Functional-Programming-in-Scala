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

  "bag" when {
    "given an empty sequence" should {
      "return empty map" in {
        val seq = IndexedSeq.empty[Int]

        val result = Chapter10.bag(seq)

        result should be(Map())
      }
    }

    "given a sequence with one element" should {
      "return map with that element mapped to 1" in {
        val seq = IndexedSeq("a")

        val result = Chapter10.bag(seq)

        result should be(Map("a" -> 1))
      }
    }

    "given a sequence with multiple occurrences of the same element" should {
      "count occurrences of that element" in {
        val seq = IndexedSeq.fill(10)("a")

        val result = Chapter10.bag(seq)

        result should be(Map("a" -> 10))
      }
    }

    "given a sequence with only distinct elements" should {
      "map every element to 1" in {
        val seq = IndexedSeq("a", "b", "c", "d")

        val result = Chapter10.bag(seq)

        val expected = seq.map(_ -> 1).toMap
        result should be(expected)
      }
    }

    "given a sequence with only some elements with multiple occurrences" should {
      "assign count to every element" in {
        val ab = IndexedSeq("a", "b")
        val bc = IndexedSeq("b", "c")
        val cd = IndexedSeq("c", "d")
        val seq = ab ++ bc ++ cd

        val result = Chapter10.bag(seq)

        val expected = Map("a" -> 1, "b" -> 2, "c" -> 2, "d" -> 1)
        result should be(expected)
      }
    }
  }
}
