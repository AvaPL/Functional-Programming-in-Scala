package chapter5

import org.scalactic.Equality
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

  "toArray" when {
    "Stream is empty" should {
      "return empty Array" in {
        Stream().toArray should be(Array())
      }
    }

    "Stream has elements" should {
      "evaluate and return an Array" in {
        Stream(1, 2, 3).toArray should be(Array(1, 2, 3))
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

  "headOption" when {
    "given empty Stream" should {
      "return None" in {
        Stream.empty.headOption should be(None)
      }
    }

    "given stream with elements" should {
      "return Some with first element" in {
        Stream(1, 2, 3).headOption should be(Some(1))
      }
    }

    "given infinite Stream" should {
      "terminate early and return first value" in {
        Stream.ones.headOption should be(Some(1))
      }
    }
  }

  "map" when {
    "given empty Stream" should {
      "return empty Stream" in {
        Stream.empty.map(_: Nothing => "test").toList should be(Stream.empty.toList)
      }
    }

    "given stream with elements" should {
      "map all elements" in {
        Stream(1, 2, 3).map(_.toString).toList should be(Stream("1", "2", "3").toList)
      }
    }

    "given infinite Stream" should {
      "map elements lazily (terminate immediately)" in {
        Stream.ones.map(_ * 2).headOption should contain(2)
      }
    }
  }

  "filter" when {
    "given empty Stream" should {
      "return empty Stream for always false predicate" in {
        Stream.empty.filter(_: Nothing => false).toList should be(Stream.empty.toList)
      }

      "return empty Stream for always true predicate" in {
        Stream.empty.filter(_: Nothing => true).toList should be(Stream.empty.toList)
      }
    }

    "given Stream with elements" should {
      "return empty Stream for always false predicate" in {
        Stream(1, 2, 3).filter(_ => false).toList should be(Stream.empty.toList)
      }

      "return initial Stream for always true predicate" in {
        Stream(1, 2, 3).filter(_ => true).toList should be(Stream(1, 2, 3).toList)
      }

      "return only matching elements" in {
        Stream(1, 2, 3).filter(_ % 2 != 0).toList should be(Stream(1, 3).toList)
      }
    }
  }

  "append" when {
    "given empty Stream" should {
      "append one element" in {
        Stream.empty.append(5).toList should be(Stream(5).toList)
      }
    }

    "given Stream with elements" should {
      "append one element" in {
        Stream(1, 2, 3, 4).append(5).toList should be(Stream(1, 2, 3, 4, 5).toList)
      }

      "allow to append element with common supertype" in {
        Stream(1, 2, 3, 4).append(5f).toList should be(Stream(1f, 2f, 3f, 4f, 5f).toList)
      }
    }

    "given infinite Stream" should {
      "append element lazily (terminate immediately)" in {
        noException should be thrownBy Stream.ones.append(5)
      }
    }
  }

  "flatMap" when {
    "given empty Stream" should {
      "return empty Stream when function always returns empty Stream" in {
        Stream.empty[Nothing].flatMap(_ => Stream.empty).toList should be(Stream.empty.toList)
      }

      "return empty Stream when function always returns Stream with elements" in {
        Stream.empty[Nothing].flatMap(_ => Stream(1, 2, 3)).toList should be(Stream.empty.toList)
      }
    }

    "given Stream with element" should {
      "return empty Stream when function always returns Stream with elements" in {
        Stream(1, 2, 3).flatMap(_ => Stream.empty).toList should be(Stream.empty.toList)
      }

      "return elements in same order when mapped to Stream with one element" in {
        Stream(1, 2, 3).flatMap(Stream(_)).toList should be(Stream(1, 2, 3).toList)
      }

      "return flattened Stream when one element is mapped to multiple elements" in {
        Stream(1, 2, 3).flatMap(i => Stream(List.fill(i)(i): _*)).toList should be(Stream(1, 2, 2, 3, 3, 3).toList)
      }

      "return elements in valid order" in {
        Stream(1, 2).flatMap(i => Stream(8, i, 9)).toList should be(Stream(8, 1, 9, 8, 2, 9).toList)
      }
    }
  }

  "constant" when {
    "given argument" should {
      "generate infinite Stream of that element" in {
        Stream.constant(5).take(100).toList should be(List.fill(100)(5))
        Stream.constant("test").take(100).toList should be(List.fill(100)("test"))
      }
    }
  }

  "from" when {
    "given an integer" should {
      "generate infinite Stream of consecutive numbers" in {
        Stream.from(5).take(5).toList should be(Stream(5, 6, 7, 8, 9).toList)
      }
    }
  }

  "fibs" when {
    "called" should {
      "generate infinite Stream of Fibonacci sequence" in {
        Stream.fibs.take(10).toList should be(Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34).toList)
      }
    }
  }

  "unfold" when {
    "function always returns None" should {
      "return empty Stream" in {
        Stream.unfold(0)(_ => None).toList should be(Stream.empty.toList)
      }
    }

    "function always returns Some" should {
      "return infinite Stream" in {
        Stream.unfold(0)(i => Some((i, i + 1))).take(10).toList should be(Stream(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).toList)
      }
    }

    "function returns None at some point" should {
      "return finite Stream" in {
        Stream.unfold(0)(i => Option.when(i < 10)((i, i + 1))).toList should be(Stream(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).toList)
      }
    }
  }

  "onesViaUnfold" should {
    "be same as ones" in {
      Stream.onesViaUnfold.take(100).toList should be(Stream.ones.take(100).toList)
    }
  }

  "constantViaUnfold" should {
    "be same as constant" in {
      Stream.constantViaUnfold(10).take(100).toList should be(Stream.constant(10).take(100).toList)
    }
  }

  "fromViaUnfold" when {
    "used" should {
      "be same as from" in {
        Stream.fromViaUnfold(5).take(100).toList should be(Stream.from(5).take(100).toList)
      }
    }

    "given double and step" should {
      "return a Stream of doubles incremented by 1.0" in {
        Stream.fromViaUnfold(5.0).take(5).toList should equal(Stream[Double](5, 6, 7, 8, 9).toList)
      }

      "return a Stream of doubles incremented by step" in {
        Stream.fromViaUnfold(5.0, 5.0).take(5).toList should equal(Stream[Double](5, 10, 15, 20, 25).toList)
      }

      "return a Stream of doubles incremented by negative step (decremented)" in {
        Stream.fromViaUnfold(5.0, -5.0).take(5).toList should equal(Stream[Double](5, 0, -5, -10, -15).toList)
      }

      "return a Stream of constant when step is set to 0.0" in {
        Stream.fromViaUnfold(5.0, 0.0).take(5).toList should equal(Stream[Double](5.0, 5.0, 5.0, 5.0, 5.0).toList)
      }

      implicit val doublesEquality: Equality[Double] =
        (a: Double, b: Any) => b match {
          case b: Double => a === b +- 0.01
          case _ => false
        }
    }
  }

  "fibsViaUnfold" should {
    "be same as fibs" in {
      Stream.fibsViaUnfold.take(100).toList should be(Stream.fibs.take(100).toList)
    }
  }

  "mapViaUnfold" should {
    "be same as map" in {
      Stream(1, 2, 3).map(_ * 2).toList should be(Stream(2, 4, 6).toList)
    }

    "be lazily evaluated" in {
      noException should be thrownBy Stream.ones.mapViaUnfold(_ * 2).take(100).toList
    }
  }

  "takeViaUnfold" should {
    "be same as take" in {
      Stream.fibs.takeViaUnfold(100).toList should be(Stream.fibs.take(100).toList)
    }
  }

  "takeWhileViaUnfold" should {
    "be same as takeWhile" in {
      Stream.fibs.takeWhileViaUnfold(_ < 1000).toList should be(Stream.fibs.takeWhile(_ < 1000).toList)
    }
  }

  "zipWith" when {
    "given two empty Streams" should {
      "return empty Stream" in {
        Stream.empty.zipWith(Stream.empty)((_: Nothing, _: Nothing) => 5).toList should be(Stream.empty.toList)
      }
    }

    "given one empty Stream" should {
      "return empty Stream for empty Stream on left" in {
        Stream.empty.zipWith(Stream(1, 2, 3))((_: Nothing, _) => 5).toList should be(Stream.empty.toList)
      }

      "return empty Stream for empty Stream on right" in {
        Stream(1, 2, 3).zipWith(Stream.empty)((_, _: Nothing) => 5).toList should be(Stream.empty.toList)
      }
    }

    "given two nonempty Streams" should {
      "zip two Streams of same length" in {
        Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _).toList should be(Stream(5, 7, 9).toList)
      }

      "zip two Streams for left shorter Stream" in {
        Stream(1).zipWith(Stream(1, 2, 3))(_ + _).toList should be(Stream(2).toList)
      }

      "zip two Streams for right shorter Stream" in {
        Stream(1, 2, 3).zipWith(Stream(2, 1))(_ + _).toList should be(Stream(3, 3).toList)
      }
    }
  }

  "zipAll" when {
    "given two empty Streams" should {
      "return empty Stream" in {
        Stream.empty.zipAll(Stream.empty).toList should be(Stream.empty.toList)
      }
    }

    "given one empty Stream" should {
      "return empty Stream for empty Stream on left" in {
        Stream.empty.zipAll(Stream(1, 2, 3)).toList should be(Stream((None, Some(1)), (None, Some(2)), (None, Some(3))).toList)
      }

      "return empty Stream for empty Stream on right" in {
        Stream(1, 2, 3).zipAll(Stream.empty).toList should be(Stream((Some(1), None), (Some(2), None), (Some(3), None)).toList)
      }
    }

    "given two nonempty Streams" should {
      "zip two Streams of same length" in {
        Stream(1, 2, 3).zipAll(Stream(4, 5, 6)).toList should be(Stream((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6))).toList)
      }

      "zip two Streams for left shorter Stream" in {
        Stream(1).zipAll(Stream(1, 2, 3)).toList should be(Stream((Some(1), Some(1)), (None, Some(2)), (None, Some(3))).toList)
      }

      "zip two Streams for right shorter Stream" in {
        Stream(1, 2, 3).zipAll(Stream(2, 1)).toList should be(Stream((Some(1), Some(2)), (Some(2), Some(1)), (Some(3), None)).toList)
      }
    }
  }

  "startsWith" when {
    "given two empty Streams" should {
      "return true" in {
        Stream.empty.startsWith(Stream.empty) should be(true)
      }
    }

    "given nonempty Stream and empty prefix" should {
      "return true" in {
        Stream(1, 2, 3).startsWith(Stream.empty) should be(true)
      }
    }

    "given nonempty Stream and nonempty prefix" should {
      "return true when Stream starts with prefix" in {
        Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3)) should be(true)
      }

      "return false when Stream does not start with prefix" in {
        Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 8, 9)) should be(false)
      }

      "return false when matching prefix is longer than Stream" in {
        Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4, 5)) should be(false)
      }

      "return true when Stream length is equal to matching prefix length" in {
        Stream(1, 2, 3).startsWith(Stream(1, 2, 3)) should be(true)
      }
    }

    "given infinite Stream and finite prefix" should {
      "terminate and return true when the prefix matches" in {
        Stream.from(1).startsWith(Stream(1, 2, 3)) should be(true)
      }

      "terminate and return false when prefix does not match" in {
        Stream.from(1).startsWith(Stream(1, 5, 6)) should be(false)
      }
    }

    "given finite Stream and infinite prefix" should {
      "terminate and return false when matching prefix is longer than Stream" in {
        Stream(1, 2, 3).startsWith(Stream.from(1)) should be(false)
      }

      "terminate and return false when the prefix does not match" in {
        Stream(1, 2, 1).startsWith(Stream.from(1)) should be(false)
      }
    }

    "given infinite Stream and infinite prefix" should {
      "terminate and return false when prefix does not match" in {
        Stream.from(1).startsWith(Stream.ones) should be(false)
      }
    }
  }

  def toListOfLists[A](stream: Stream[Stream[A]]): List[List[A]] = stream.toList.map(_.toList)

  "tails" when {
    "given empty Stream" should {
      "return Stream with one empty Stream element" in {
        toListOfLists(Stream.empty.tails) should be(toListOfLists(Stream(Stream.empty)))
      }
    }

    "given nonempty Stream" should {
      "return tails for one element" in {
        toListOfLists(Stream(1).tails) should be(toListOfLists(Stream(Stream(1), Stream.empty)))
      }

      "return tails for multiple elements" in {
        toListOfLists(Stream(1, 2, 3).tails) should be(toListOfLists(Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream.empty)))
      }
    }

    "given infinite Stream" should {
      "terminate and return tails" in {
        val tails = Stream.from(1).tails
        val secondTail = tails.drop(1).headOption.get

        val expectedSecondTail = Stream.from(2)
        secondTail.take(100).toList should be(expectedSecondTail.take(100).toList)
      }
    }
  }

  "scanRight" when {
    "given empty Stream" should {
      "return initial accumulator value" in {
        Stream.empty.scanRight(0)((_: Nothing, _) => 5).toList should be(Stream(0).toList)
      }
    }

    "given nonempty Stream" should {
      "scan and return intermediate results for one type" in {
        Stream(1, 1, 1).scanRight(0)(_ + _).toList should be(Stream(3, 2, 1, 0).toList)
      }

      "scan and return intermediate results for multiple types" in {
        Stream(1, 1, 1).scanRight(0.0)((a, b) => (a + b).toFloat).toList should be(Stream(3, 2, 1, 0).toList)
      }
    }

    "elements are never evaluated" should {
      "not overflow" in {
        noException should be thrownBy Stream.ones.scanRight(0)(_ + _)
      }
    }
  }

  "iterate" when {
    "given start element and a function" should {
      "consecutively apply function" in {
        Stream.iterate(1)(_ * 2).take(5).toList should be(Stream(1, 2, 4, 8, 16).toList)
      }
    }
  }
}
