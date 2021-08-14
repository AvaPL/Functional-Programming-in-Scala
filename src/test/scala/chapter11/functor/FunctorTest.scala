package chapter11.functor

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class FunctorTest extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "list" when {
    "identity function is used" should {
      "not change the input" in {
        forAll { list: List[Int] =>
          Functor.list.map(list)(identity) should be(list)
        }
      }
    }
  }
}
