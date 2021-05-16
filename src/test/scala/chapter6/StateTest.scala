package chapter6

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import State._

class StateTest extends AnyWordSpec with Matchers {
  "flatMap" should {
    "apply a function to result of initial state run" in {
      val initial = State[Int, String](i => (i.toString, i + 1))
      val function = (s: String) => State[Int, String](i => (s + i.toString, i + 2))

      val (result, state) = initial.flatMap(function).run(5)

      result should be("56")
      state should be(8)
    }
  }

  "map" should {
    "map value of state using function" in {
      val state = State[Int, String](i => (i.toString, i + 1))

      val result = state.map(_.length)

      result.run(111) should be((3, 112))
    }
  }

  "sequence" when {
    val run = (i: Int) => (i.toString, i + 1)

    "given empty list" should {
      "return Rand that returns an empty list" in {
        sequence(Nil).run(0) should be((Nil, 0))
      }
    }

    "given list with one element" should {
      "return rand with one element in list" in {
        val state = State(run)
        val (expectedValue, expectedState) = state.run(0)

        val (result, resultState) = sequence(List(state)).run(0)

        result should be(List(expectedValue))
        resultState should be(expectedState)
      }
    }

    "given list with multiple elements" should {
      "return elements in list in correct order" in {
        val state = State(run)
        val (expectedValue1, expectedState1) = state.run(0)
        val (expectedValue2, expectedState2) = state.run(expectedState1)
        val (expectedValue3, expectedState3) = state.run(expectedState2)

        val (result, resultRng) = sequence(List(state, state, state)).run(0)

        result should be(List(expectedValue1, expectedValue2, expectedValue3))
        resultRng should be(expectedState3)
      }
    }
  }
}
