package chapter5

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StreamTest extends AnyWordSpec with Matchers {

  "toList" when {
    "Stream is empty" should {
      "return Nil" in {
        Stream().toList should be(Nil)
      }
    }

    "Stream has elements" should {
      "evaluate and return a List" in {
        Stream(1, 2, 3).toList should be(List(1, 2, 3))
      }
    }
  }

  "take" when {
    "given negative number" should {
      "return empty Stream when called on empty Stream" in {
        Stream.empty.take(-5).toList should be(Stream.empty.toList)
      }

      "return empty Stream when called on nonempty Stream" in {
        Stream(1, 2, 3).take(-5).toList should be(Stream.empty.toList)
      }
    }

    "given 0" should {
      "return empty Stream when called on empty Stream" in {
        Stream.empty.take(0).toList should be(Stream.empty.toList)
      }

      "return empty Stream when called on nonempty Stream" in {
        Stream(1, 2, 3).take(0).toList should be(Stream.empty.toList)
      }
    }

    "given positive number" should {
      "return empty Stream when called on empty Stream" in {
        Stream.empty.take(2).toList should be(Stream.empty.toList)
      }

      "take given number of elements of Stream" in {
        Stream(1, 2, 3).take(2).toList should be(Stream(1, 2).toList)
      }

      "return all elements if number is equal to Stream length" in {
        Stream(1, 2, 3).take(3).toList should be(Stream(1, 2, 3).toList)
      }

      "return all elements if number is bigger than Stream length" in {
        Stream(1, 2, 3).take(5).toList should be(Stream(1, 2, 3).toList)
      }
    }

    "given large number" should {
      "not overflow" in {
        noException should be thrownBy Stream.ones.take(1000000).toList
      }
    }
  }

  "drop" when {
    "given negative number" should {
      "return empty Stream when called on empty Stream" in {
        Stream.empty.drop(-5).toList should be(Stream.empty.toList)
      }

      "return initial Stream when called on nonempty Stream" in {
        Stream(1, 2, 3).drop(-5).toList should be(Stream(1, 2, 3).toList)
      }
    }

    "given 0" should {
      "return empty Stream when called on empty Stream" in {
        Stream.empty.drop(0).toList should be(Stream.empty.toList)
      }

      "return initial Stream when called on nonempty Stream" in {
        Stream(1, 2, 3).drop(0).toList should be(Stream(1, 2, 3).toList)
      }
    }

    "given positive number" should {
      "return empty Stream when called on empty Stream" in {
        Stream.empty.drop(2).toList should be(Stream.empty.toList)
      }

      "drop given number of elements of Stream" in {
        Stream(1, 2, 3).drop(2).toList should be(Stream(3).toList)
      }

      "return empty Stream if number is equal to Stream length" in {
        Stream(1, 2, 3).drop(3).toList should be(Stream.empty.toList)
      }

      "return empty Stream if number is bigger than Stream length" in {
        Stream(1, 2, 3).drop(5).toList should be(Stream.empty.toList)
      }
    }

    "given large number" should {
      "not overflow" in {
        noException should be thrownBy Stream.ones.drop(1000000).take(0).toList
      }
    }
  }

  "takeWhile" when {
    "given empty Stream" should {
      "return empty Stream for always false predicate" in {
        Stream.empty.takeWhile(_: Nothing => false).toList should be(Stream.empty.toList)
      }

      "return empty Stream for always true predicate" in {
        Stream.empty.takeWhile(_: Nothing => true).toList should be(Stream.empty.toList)
      }
    }

    "given Stream with elements" should {
      "return empty Stream for always false predicate" in {
        Stream(1, 2, 3).takeWhile(_ => false).toList should be(Stream.empty.toList)
      }

      "return initial Stream for always true predicate" in {
        Stream(1, 2, 3).takeWhile(_ => true).toList should be(Stream(1, 2, 3).toList)
      }

      "return part of the Stream that matches predicate" in {
        Stream(1, 2, 3, 2, 2).takeWhile(_ < 3).toList should be(Stream(1, 2).toList)
      }
    }
  }

  "forAll" when {
    "used on empty Stream" should {
      "return true for always false predicate" in {
        Stream.empty.forAll(_: Nothing => false) should be(true)
      }

      "return true for always true predicate" in {
        Stream.empty.forAll(_: Nothing => true) should be(true)
      }
    }

    "used on nonempty Stream" should {
      "return false for always false predicate" in {
        Stream(1, 2, 3).forAll(_ => false) should be(false)
      }

      "return true for always true predicate" in {
        Stream(1, 2, 3).forAll(_ => true) should be(true)
      }

      "return false for predicate that doesn't match all elements" in {
        Stream(2, 4, 6, 7, 4).forAll(_ % 2 == 0) should be(false)
      }
    }

    "used on infinite Stream" should {
      "terminate early when as soon as predicate is false" in {
        Stream.ones.forAll(_ == 2) should be(false)
      }
    }
  }
}
