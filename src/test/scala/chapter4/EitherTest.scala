package chapter4

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EitherTest extends AnyWordSpec with Matchers {

  "map" when {
    "given Left" should {
      "return Left" in {
        Left(5).map(_ => 10) should be(Left(5))
      }
    }

    "given Right" should {
      "map value" in {
        Right(5).map(_.toString) should be(Right("5"))
      }
    }
  }

  "flatMap" when {
    "given Left" should {
      "return Left" in {
        Left(5).flatMap(_ => Right(10)) should be(Left(5))
      }
    }

    "given Right" should {
      "return Right when function returns Right" in {
        Right(5).flatMap(i => Right(i.toString)) should be(Right("5"))
      }

      "return Left when function returns Left" in {
        Right(5).flatMap(i => Left(i.toString)) should be(Left("5"))
      }
    }
  }

  "orElse" when {
    "given Left" should {
      "return default Right value" in {
        Left(5).orElse(Right(10)) should be(Right(10))
      }

      "return default Left value" in {
        Left(5).orElse(Left(10)) should be(Left(10))
      }
    }

    "given Right" should {
      "return primary Right when default is Right" in {
        Right(5).orElse(Right(10)) should be(Right(5))
      }

      "return primary Right when default is Left" in {
        Right(5).orElse(Left(10)) should be(Right(5))
      }
    }
  }

  "map2" when {
    val throwable = new Throwable("test")

    "given two Right" should {
      "return Right with mapped value" in {
        Either.map2(Right(5), Right(10))((a, b) => (a + b).toString) should be(Right("15"))
      }
    }

    "given one Left" should {
      "return Left for Left on the left" in {
        Either.map2(Left(throwable): Either[Throwable, Int], Right(10))((a, b) => (a + b).toString) should be(Left(throwable))
      }

      "return Left for Left on the right" in {
        Either.map2(Right(10), Left(throwable): Either[Throwable, Int])((a, b) => (a + b).toString) should be(Left(throwable))
      }
    }

    "given two Lefts" should {
      "return Left on the left" in {
        val otherThrowable = new Throwable("other")
        Either.map2(Left(throwable): Either[Throwable, Int], Left(otherThrowable): Either[Throwable, Int])((a, b) => (a + b).toString) should be(Left(throwable))
      }
    }
  }

  "traverse" should {
    "return Right for Nil" in {
      Either.traverse(Nil)(_ => Right(5)) should be(Right(Nil))
    }

    "return Right for list of Rights" in {
      Either.traverse(List(1, 2, 3))(i => Right(i.toString)) should be(Right(List("1", "2", "3")))
    }

    "return Left for one Left in the list" in {
      Either.traverse(List(1, 2, 3))(i => if (i == 2) Left("left") else Right(i.toString)) should be(Left("left"))
    }

    "return first Left for only Lefts in the list" in {
      Either.traverse(List(1, 2, 3))(i => Left(i.toString)) should be(Left("1"))
    }
  }

  "sequence" should {
    "return Right for Nil" in {
      Either.sequence(Nil) should be(Right(Nil))
    }

    "return Right for list of Rights" in {
      Either.sequence(List(Right(1), Right(2), Right(3))) should be(Right(List(1, 2, 3)))
    }

    "return Left for one Left in the list" in {
      Either.sequence(List(Right(1), Left("left"), Right(3))) should be(Left("left"))
    }

    "return first Left for list of Lefts" in {
      Either.sequence(List(Left(1), Left(2), Left(3))) should be(Left(1))
    }
  }
}
