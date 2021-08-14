package chapter11.monad

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MonadTest extends AnyWordSpec with Matchers {
  "filterM" when {
    "used with List monad" should {
      "return Nil when no booleans are supplied" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.list.filterM(list)(_ => Nil)

        result should be(Nil)
      }

      "behave like standard filter for only one boolean" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.list.filterM(list)(i => List(i % 2 == 0))

        result should be(List(List(2, 4)))
      }

      "return Nil when certain values return None" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.list.filterM(list)(i => Option.when(i % 2 != 0)(i % 3 != 0).toList)

        result should be(Nil)
      }

      // When list is bigger than 1 it becomes too complex to understand or use.
      // Leaving it without tests.
    }

    "used with Option monad" should {
      "return None when None boolean is always supplied" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.option.filterM(list)(_ => None)

        result should be(None)
      }

      "behave like standard filter for only one boolean" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.option.filterM(list)(i => Some(i % 2 == 0))

        result should be(Some(List(2, 4)))
      }

      "return None when certain values return None" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.option.filterM(list)(i => Option.when(i % 2 != 0)(i % 3 != 0))

        result should be(None)
      }
    }
  }
}
