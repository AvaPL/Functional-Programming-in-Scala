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
}
