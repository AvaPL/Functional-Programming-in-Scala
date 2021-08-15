package chapter11.monad

import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.reflect.ClassTag

class MonadTest extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {
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

  "compose" when {
    "used on two functions" should {
      "create a composed function" in {
        val function1 = (i: Int) => Some(i.toString)
        val function2 = (s: String) => Some(s * 3)
        val composed = Monad.option.compose(function1, function2)

        val result = composed(5)

        result should be(Some("555"))
      }
    }
  }

  // TODO: Check other monads
  checkAssociativeLaw(Monad.option)((i: Int) => Some(i.toString))(_.headOption)(_.toString.toIntOption)

  def checkAssociativeLaw[F[_], A, B, C, D]
  (monad: Monad[F])
  (f: A => F[B])
  (g: B => F[C])
  (h: C => F[D])
  (implicit monadType: ClassTag[F[_]], arbitrary: Arbitrary[A]): Unit = {
    s"Monad[$monadType]" should {
      "obey associative law" in {
        import monad.compose
        forAll { a: A =>
          compose(compose(f, g), h)(a) should be(compose(f, compose(g, h))(a))
        }
      }
    }
  }
}