package chapter3

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListTest extends AnyWordSpec with Matchers {

  "List" when {
    "used in pattern matching" should {
      "return 3" in {
        val result = List(1, 2, 3, 4, 5) match {
          case Cons(x, Cons(2, Cons(4, _))) => x
          case Nil => 42
          case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
          case Cons(h, t) => h + List.sum(t)
          case _ => 101
        }

        result should be(3)
      }
    }
  }

  "tail" when {
    "given Nil" should {
      "return Nil" in {
        List.tail(Nil) should be(Nil)
      }
    }

    "given one element" should {
      "return Nil" in {
        List.tail(List(1)) should be(Nil)
      }
    }

    "given more than one element" should {
      "return tail" in {
        List.tail(List(1, 3, 2)) should be(List(3, 2))
      }
    }
  }

  "setHead" when {
    "given Nil" should {
      "return list with one element" in {
        List.setHead(Nil)(1) should be(List(1))
      }
    }

    "given one element" should {
      "replace element" in {
        List.setHead(List(5))(2) should be(List(2))
      }
    }

    "given more than one element" should {
      "replace head" in {
        List.setHead(List(1, 3, 4))(7) should be(List(7, 3, 4))
      }
    }
  }

  "drop" when {
    "given Nil" should {
      "return Nil" in {
        List.drop(List(1, 2, 3), 5) should be(Nil)
      }
    }

    "given 3 elements" should {
      "drop first 2" in {
        List.drop(List(5, 2, 1), 2) should be(List(1))
      }
    }

    "given n=0" should {
      "not drop anything" in {
        List.drop(List(4, 2, 2), 0) should be(List(4, 2, 2))
      }
    }

    "given n greater than list length" should {
      "return Nil" in {
        List.drop(List(1, 2, 3), 5) should be(Nil)
      }
    }
  }

  "dropWhile" when {
    "given Nil" should {
      "return Nil for always false predicate" in {
        List.dropWhile(Nil)(_: Nothing => false) should be(Nil)
      }

      "return Nil for always true predicate" in {
        List.dropWhile(Nil)(_: Nothing => false) should be(Nil)
      }
    }

    "given multiple elements" should {
      "return Nil for always true predicate" in {
        List.dropWhile(List(1, 2, 3))(_ => true) should be(Nil)
      }

      "drop first 2 elements that match the predicate" in {
        List.dropWhile(List(1, 2, 3, 4, 5))(_ < 3) should be(List(3, 4, 5))
      }

      "stop dropping after first not matching element" in {
        List.dropWhile(List(1, 1, 2, 1, 1))(_ == 1) should be(List(2, 1, 1))
      }
    }
  }

  "init" when {
    "given Nil" should {
      "return Nil" in {
        List.init(Nil) should be(Nil)
      }
    }

    "given one element" should {
      "return Nil" in {
        List.init(List(1)) should be(Nil)
      }
    }

    "given multiple elements" should {
      "return all except last one" in {
        List.init(List(4, 3, 2, 1)) should be(List(4, 3, 2))
      }
    }
  }

  "length" when {
    "given Nil" should {
      "return 0" in {
        List.length(Nil) should be(0)
      }
    }

    "given one element" should {
      "return 1" in {
        List.length(List(1)) should be(1)
      }
    }

    "given multiple elements" should {
      "return list length" in {
        List.length(List(1, 2, 4, 2, 1)) should be(5)
      }
    }
  }

  "foldRight" when {
    "used with Cons" should {
      "return same list" in {
        List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should be(List(1, 2, 3))
      }
    }
  }

  "foldLeft" when {
    "given Nil" should {
      "immediately return accumulator" in {
        List.foldLeft(Nil: List[Int], 5)(_ + _) should be(5)
      }
    }

    "given list of numbers" should {
      "sum numbers" in {
        List.foldLeft(List(1, 2, 4, 1, 2), 0)(_ + _) should be(10)
      }

      "return product" in {
        List.foldLeft(List(5.0, 4.0, 0.5), 1.0)(_ * _)
      }
    }

    "given large input" should {
      "be stack safe" in {
        var list: List[Int] = Nil
        for (_ <- 0 to 10000)
          list = Cons(1, list)

        a[StackOverflowError] should be thrownBy List.foldRight(list, 0)(_ * _)
        List.foldLeft(list, 1)(_ * _) should be(1)
      }
    }
  }

  "reverse" when {
    "given Nil" should {
      "return Nil" in {
        List.reverse(Nil) should be(Nil)
      }
    }

    "given one element" should {
      "return that element" in {
        List.reverse(List(1)) should be(List(1))
      }
    }

    "given multiple elements" should {
      "return reversed list" in {
        List.reverse(List(1, 2, 3)) should be(List(3, 2, 1))
      }
    }
  }

  "foldRight2" when {
    "given a list" should {
      "return same results as foldRight" in {
        val list = List(1, 2, 4)
        val difference = (a: Int, b: Int) => b - a
        val division = (a: Int, b: Int) => b / a

        List.foldRight2(list, 0)(difference) should be(List.foldRight(list, 0)(difference))
        List.foldRight2(list, 16)(division) should be(List.foldRight(list, 16)(division))
      }

      "be stack safe" in {
        var list: List[Int] = Nil
        for (_ <- 0 to 10000)
          list = Cons(1, list)

        noException should be thrownBy List.foldRight2(list, 0)(_ * _)
      }
    }
  }

  "append" when {

    val nil = Nil: List[Int]

    "given empty list" should {
      "not change when appending Nil" in {
        List.append(nil, nil) should be(nil)
      }

      "add one element" in {
        List.append(nil, List(4)) should be(List(4))
      }

      "add multiple elements" in {
        List.append(nil, List(4, 5)) should be(List(4, 5))
      }
    }

    "given non empty list" should {
      "not change when appending Nil" in {
        List.append(List(1, 2, 3), nil) should be(List(1, 2, 3))
      }

      "add one element" in {
        List.append(List(1, 2, 3), List(4)) should be(List(1, 2, 3, 4))
      }

      "add multiple elements" in {
        List.append(List(1, 2, 3), List(4, 5)) should be(List(1, 2, 3, 4, 5))
      }
    }
  }

  "concat" when {
    "given multiple lists" should {
      "concat lists" in {
        List.concat(List(Nil, List(1, 2), Nil, List(3, 4), Nil)) should be(List(1, 2, 3, 4))
      }
    }
  }

  "add1" when {
    "given Nil" should {
      "return Nil" in {
        List.add1(Nil) should be(Nil)
      }
    }

    "given one element" should {
      "return list with element incremented" in {
        List.add1(List(1)) should be(List(2))
      }
    }

    "given multiple elements" should {
      "return list with each element incremented" in {
        List.add1(List(1, 2, 3)) should be(List(2, 3, 4))
      }
    }
  }

  "doubleToString" when {
    "given Nil" should {
      "return Nil" in {
        List.doubleToString(Nil) should be(Nil)
      }
    }

    "given one element" should {
      "return list with element converted to string" in {
        List.doubleToString(List(1.0)) should be(List("1.0"))
      }
    }

    "given multiple elements" should {
      "return list with each element converted to string" in {
        List.doubleToString(List(1.1, 2.52, 3.123)) should be(List("1.1", "2.52", "3.123"))
      }
    }
  }

  "map" when {
    "given Nil" should {
      "return Nil" in {
        List.map(Nil)(identity) should be(Nil)
      }
    }

    "given one element" should {
      "return list with element squared" in {
        List.map(List(0.5))(x => x * x) should be(List(0.25))
      }

      "return list with negative element" in {
        List.map(List(1))(-_) should be(List(-1))
      }
    }

    "given multiple elements" should {
      "return list with each element squared" in {
        List.map(List(1, 5, 0.5))(x => x * x) should be(List(1, 25, 0.25))
      }

      "return list with each element negated" in {
        List.map(List(1, 5, -3))(-_) should be(List(-1, -5, 3))
      }
    }
  }

  "filter" when {
    "given Nil" should {
      "return Nil" in {
        List.filter(Nil)(_ => true) should be(Nil)
      }
    }

    "given multiple elements" should {
      "not change list when predicate matches all elements" in {
        List.filter(List(1, 5, 3))(_ < 10) should be(List(1, 5, 3))
      }

      "return Nil when no elements match predicate" in {
        List.filter(List(1, 5, 3))(_ > 10) should be(Nil)
      }

      "return odd elements" in {
        List.filter(List(1, -2, -3, 4, 5))(_ % 2 != 0) should be(List(1, -3, 5))
      }
    }
  }

  "flatMap" when {
    "given a list" should {
      "flatten the result" in {
        val result = List.flatMap(List(1, 2, 3, 4, 5)) {
          case 1 => Nil
          case 3 => List(3)
          case 5 => Nil
          case i => List(i, i)
        }

        result should be(List(2, 2, 3, 4, 4))
      }
    }
  }

  "addPairwise" when {
    "given two Nils" should {
      "return Nil" in {
        List.addPairwise(Nil, Nil) should be(Nil)
      }
    }

    "given one Nil list" should {
      "return left list" in {
        List.addPairwise(List(1, 2, 3), Nil) should be(List(1, 2, 3))
      }

      "return right list" in {
        List.addPairwise(Nil, List(1, 2, 3)) should be(List(1, 2, 3))
      }
    }

    "given lists of equal sizes" should {
      "sum corresponding elements" in {
        List.addPairwise(List(1, 2, 3), List(5, 3, 1)) should be(List(6, 5, 4))
      }
    }

    "given one longer list" should {
      "sum corresponding elements and return remaining left list elements" in {
        List.addPairwise(List(1, 2, 3), List(5)) should be(List(6, 2, 3))
      }

      "sum corresponding elements and return remaining right list elements" in {
        List.addPairwise(List(5), List(1, 2, 3)) should be(List(6, 2, 3))
      }
    }
  }

  "zipWith" when {
    "given two Nils" should {
      "return Nil" in {
        List.zipWith(Nil, Nil)((_, _) => 0) should be(Nil)
      }
    }

    "given one Nil list" should {
      "return Nil for right Nil list" in {
        List.zipWith(List(1, 2, 3), Nil)((_, _) => 0) should be(Nil)
      }

      "return Nil for left Nil list" in {
        List.zipWith(Nil, List(1, 2, 3))((_, _) => 0) should be(Nil)
      }
    }

    "given lists of equal sizes" should {
      "sum corresponding elements" in {
        List.zipWith(List(1, 2, 3), List(5, 3, 1))(_ + _) should be(List(6, 5, 4))
      }
    }

    "given one longer list" should {
      "sum corresponding elements for right shorter list" in {
        List.zipWith(List(1, 2, 3), List(5, 3))(_ + _) should be(List(6, 5))
      }

      "sum corresponding elements for left shorter list" in {
        List.zipWith(List(5, 3), List(1, 2, 3))(_ + _) should be(List(6, 5))
      }
    }
  }

  "hasSubsequence" when {
    "list does not contain subsequence" should {
      "return false" in {
        List.hasSubsequence(List(1, 2, 3, 4), List(3, 2)) should be(false)
        List.hasSubsequence(Nil, List(1, 2)) should be(false)
      }
    }

    "list contains subsequence" should {
      "return true" in {
        List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) should be(true)
        List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be(true)
        List.hasSubsequence(List(1, 2, 3, 4), List(4)) should be(true)
        List.hasSubsequence(List(1, 2, 3, 4), Nil) should be(true)
        List.hasSubsequence(Nil, Nil) should be(true)
      }
    }
  }
}
