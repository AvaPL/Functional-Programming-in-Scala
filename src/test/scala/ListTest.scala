import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListTest extends AnyWordSpec with Matchers {

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
}
