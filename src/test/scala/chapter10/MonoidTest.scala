package chapter10

import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.reflect.ClassTag

class MonoidTest extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  checkMonoid(Monoid.string)(_ + _)

  checkMonoid(Monoid.list[Int])(_ ++ _)

  checkMonoid(Monoid.intAddition)(_ + _)

  checkMonoid(Monoid.intMultiplication)(_ * _)

  checkMonoid(Monoid.booleanOr)(_ || _)

  checkMonoid(Monoid.booleanAnd)(_ && _)

  checkMonoid(Monoid.option[Int])(_.orElse(_))

  def checkMonoid[A: ClassTag](monoid: Monoid[A])(afterOp: (A, A) => A)(implicit arbitrary: Arbitrary[A]): Unit = {
    val monoidType = implicitly[ClassTag[A]].runtimeClass.getSimpleName
    s"Monoid[$monoidType] - ${monoid.getClass.getName}" when {
      "op is used on two values" should {
        "return their concatenation" in {
          forAll { (a1: A, a2: A) =>
            monoid.op(a1, a2) should be(afterOp(a1, a2))
          }
        }
      }

      "zero is used left" should {
        "not change right operand" in {
          forAll { (a2: String) =>
            val zero = Monoid.string.zero
            Monoid.string.op(zero, a2) should be(a2)
          }
        }
      }

      "zero is used right" should {
        "not change left operand" in {
          forAll { (a1: String) =>
            val zero = Monoid.string.zero
            Monoid.string.op(a1, zero) should be(a1)
          }
        }
      }
    }
  }

  "endofunction" when {
    "op is used on two functions" should {
      "return their concatenation" in {
        def a1(int: Int): Int = int + 10

        def a2(int: Int): Int = int * 10

        // (5 + 10) * 10 = 150
        Monoid.endofunction.op(a1, a2)(5) should be(150)
      }
    }

    "zero is used left" should {
      "use only right function" in {
        val zero = Monoid.endofunction[Int].zero

        def a2(int: Int): Int = int * 10

        // 5 * 10 = 10
        Monoid.endofunction.op(zero, a2)(5) should be(50)
      }
    }

    "zero is used right" should {
      "use only left function" in {
        val zero = Monoid.endofunction[Int].zero

        def a1(int: Int): Int = int + 10

        // 5 + 10 = 15
        Monoid.endofunction.op(a1, zero)(5) should be(15)
      }
    }
  }
}
