package chapter6.mystate

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MyStateTest extends AnyWordSpec with Matchers {
  "MyState" should {
    "be convertible to MyStateOps" in {
      val state = (i: Int) => (i.toString, i + 1)

      val mappedState = state.map(_ * 3) // implicit conversion

      mappedState(0) should be(("000", 1))
    }
  }
}
