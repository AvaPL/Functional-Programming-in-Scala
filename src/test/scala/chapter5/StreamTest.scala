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
}
