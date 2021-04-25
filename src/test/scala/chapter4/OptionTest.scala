package chapter4

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OptionTest extends AnyWordSpec with Matchers {

  "map" when {
    "applied to None" should {
      "return None" in {
        None.map(_ => 5) should be(None)
      }
    }

    "applied to Some" should {
      "return Some with mapped value" in {
        Some(5).map(_.toString) should be(Some("5"))
      }
    }
  }

  "getOrElse" when {
    "applied to None" should {
      "return default value" in {
        None.getOrElse(5) should be(5)
      }
    }

    "applied to Some" should {
      "return stored value instead of default" in {
        Some(5).getOrElse(10) should be(5)
      }
    }
  }

  "flatMap" when {
    "applied to None" should {
      "return None" in {
        None.flatMap(_ => Some(5)) should be(None)
      }
    }

    "applied to Some" should {
      "return Some when inner function returns Some" in {
        Some(5).flatMap(i => Some(i.toString)) should be(Some("5"))
      }

      "return None when inner function returns None" in {
        Some(5).flatMap(_ => None) should be(None)
      }
    }
  }

  "orElse" when {
    "applied to None" should {
      "return None when other is also None" in {
        None.orElse(None) should be(None)
      }

      "return Some when other is Some" in {
        None.orElse(Some(5)) should be(Some(5))
      }
    }

    "applied to Some" should {
      "return Some when other is None" in {
        Some(5).orElse(None) should be(Some(5))
      }

      "return Some with stored value when other is also Some" in {
        Some(5).orElse(Some(10)) should be(Some(5))
      }
    }
  }

  "filter" when {
    "applied to None" should {
      "return None when condition evaluates to false" in {
        None.filter(_ => false) should be(None)
      }

      "return None when condition evaluates to true" in {
        None.filter(_ => true) should be(None)
      }
    }

    "applied to Some" should {
      "return None when condition evaluates to false" in {
        Some(5).filter(_ > 10) should be(None)
      }

      "return Some when condition evaluates to true" in {
        Some(5).filter(_ < 10) should be(Some(5))
      }
    }
  }

  "map2" when {
    "given two None" should {
      "return None" in {
        Option.map2(None, None)((_, _) => 5) should be(None)
      }
    }

    "given one None" should {
      "return None for left None" in {
        Option.map2(None, Some(5))((_, _) => 5) should be(None)
      }

      "return None for right None" in {
        Option.map2(Some(5), None)((_, _) => 5) should be(None)
      }
    }

    "given two Some" should {
      "return mapped value" in {
        Option.map2(Some(5), Some(4.2))((a, b) => (a + b).toString) should be(Some("9.2"))
      }
    }
  }

  "sequence" when {
    "given Nil" should {
      "return Some(Nil)" in {
        Option.sequence(Nil) should be(Some(Nil))
      }
    }

    "given list with one element" should {
      "return None for List(None)" in {
        Option.sequence(List(None)) should be(None)
      }

      "return Some for List(Some)" in {
        Option.sequence(List(Some(5))) should be(Some(List(5)))
      }
    }

    "given list with multiple elements" should {
      "return None for list of Nones" in {
        Option.sequence(List(None, None, None)) should be(None)
      }

      "return None for list with one None" in {
        Option.sequence(List(Some(5), None, Some(10))) should be(None)
      }

      "return Some for list with Somes" in {
        Option.sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
      }
    }
  }

  "traverse" when {
    "given a list of values" should {
      "return Some when function returns Some for all elements" in {
        Option.traverse(List(1, 2, 3))(i => Some(i.toString)) should be(Some(List("1", "2", "3")))
      }

      "return None when function return None for at least one element" in {
        Option.traverse(List(1, 2, 3))(i => if (i == 2) None else Some(i)) should be(None)
      }
    }
  }
}
