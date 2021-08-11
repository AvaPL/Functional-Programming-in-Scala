package chapter10

import chapter7.Nonblocking.Par
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.util.concurrent.Executors
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

  "concatenate" when {
    "given a list of strings" should {
      "concatenate them with string monoid" in {
        val strings = List("quick", "brown", "fox")

        val result = Monoid.concatenate(strings, Monoid.string)

        result should be(strings.mkString)
      }
    }
  }

  "foldMap" when {
    "given a list of ints" should {
      "concatenate ints as strings" in {
        val ints = List(1, 3, 5)

        val result = Monoid.foldMap(ints, Monoid.string)(_.toString)

        result should be(ints.mkString)
      }
    }

    "given indexed sequence of ints" should {
      "concatenate ints as string" in {
        val ints = IndexedSeq(1, 3, 5)

        val result = Monoid.foldMap(ints, Monoid.string)(_.toString)

        result should be(ints.mkString)
      }
    }
  }

  "foldRight" when {
    "used on a list" should {
      "concatenate strings from right" in {
        val strings = List("a", "b", "c")

        val result = Monoid.foldRight(strings)("")(_ + _)

        result should be("cba")
      }
    }
  }

  "foldLeft" when {
    "used on a list" should {
      "concatenate strings from left" in {
        val strings = List("a", "b", "c")

        val result = Monoid.foldLeft(strings)("")(_ + _)

        result should be("abc")
      }
    }
  }

  "par" when {
    "given a monoid" should {
      "return a product of ints in list" in {
        val strings = List(2, 3, 4).map(Par.unit)
        val monoid = Monoid.par(Monoid.intMultiplication)

        val result = Monoid.concatenate(strings, monoid)

        val executorService = Executors.newFixedThreadPool(2)
        Par.run(executorService)(result) should be(2 * 3 * 4)
      }
    }
  }

  "parFoldMap" when {
    "given indexed sequence" should {
      "concatenate ints as string" in {
        val ints = IndexedSeq(1, 3, 5).map(Par.unit)

        val result = Monoid.parFoldMap(ints, Monoid.string)(_.toString)

        val executorService = Executors.newFixedThreadPool(2)
        Par.run(executorService)(result) should be(ints.mkString)
      }
    }
  }
}
