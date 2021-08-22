package chapter12.applicative1

import chapter12.monad.Monad
import chapter12.validation.{Failure, Success, Validation}
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ApplicativeTest extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "validation" when {
    "map2 is used on two Successes" should {
      "return a combination of values" in {
        val success1 = Success("a")
        val success2 = Success("b")

        val result = Applicative.validation.map2(success1, success2)(_ + _)

        result should be(Success("ab"))
      }
    }

    "map2 is used on Failure and Success" should {
      "return the Failure from left" in {
        val failure: Validation[String, String] = Failure("failure")
        val success = Success("test")

        val result = Applicative.validation.map2(failure, success)(_ + _)

        result should be(failure)
      }

      "return the Failure from right" in {
        val success = Success("test")
        val failure = Failure("failure")

        val result = Applicative.validation.map2(success, failure)(_ + _)

        result should be(failure)
      }
    }

    "map2 is used on two Failures" should {
      "combine the errors from failures" in {
        val failure1: Validation[String, String] = Failure("a", "b", "c")
        val failure2 = Failure("d", "e")

        val result = Applicative.validation.map2(failure1, failure2)(_ + _)

        val expected = Failure("a", "b", "c", "d", "e")
        result should be(expected)
      }
    }

    "laws" should {
      "obey identity law" in {
        checkIdentityLaw(
          applicative = Applicative.validation[String]
        )(
          gen = validationGen(Gen.long, Gen.asciiStr)
        )
      }

      "obey associativity law" in {
        checkAssociativityLaw(
          applicative = Applicative.validation[String]
        )(
          gen1 = validationGen(Gen.long, Gen.asciiStr),
          gen2 = validationGen(Gen.asciiChar, Gen.numStr),
          gen3 = validationGen(Gen.double, Gen.hexStr)
        )
      }

      "obey naturality law" in {
        checkNaturalityLaw(
          applicative = Applicative.validation[String]
        )(
          gen1 = validationGen(Gen.long, Gen.asciiStr),
          gen2 = validationGen(Gen.asciiChar, Gen.numStr)
        )(
          genF = Gen.function1[Long, String](Gen.numStr),
          genG = Gen.function1[Char, Int](Gen.size)
        )
      }
    }
  }

  "product" when {
    "used on two applicatives" should {
      val product = Monad.option.product(Applicative.validation[String])

      "obey identity law" in {
        checkIdentityLaw(
          applicative = product
        )(
          gen = tupleGen(Gen.option(Gen.long), validationGen(Gen.long, Gen.asciiStr))
        )
      }

      "obey associativity law" in {
        checkAssociativityLaw(
          applicative = product
        )(
          gen1 = tupleGen(Gen.option(Gen.long), validationGen(Gen.long, Gen.asciiStr)),
          gen2 = tupleGen(Gen.option(Gen.asciiChar), validationGen(Gen.asciiChar, Gen.numStr)),
          gen3 = tupleGen(Gen.option(Gen.double), validationGen(Gen.double, Gen.hexStr))
        )
      }

      "obey naturality law" in {
        checkNaturalityLaw(
          applicative = product
        )(
          gen1 = tupleGen(Gen.option(Gen.long), validationGen(Gen.long, Gen.asciiStr)),
          gen2 = tupleGen(Gen.option(Gen.asciiChar), validationGen(Gen.asciiChar, Gen.numStr))
        )(
          genF = Gen.function1[Long, String](Gen.numStr),
          genG = Gen.function1[Char, Int](Gen.size)
        )
      }

      def tupleGen[T, U](gen1: Gen[T], gen2: Gen[U]): Gen[(T, U)] =
        for {
          _1 <- gen1
          _2 <- gen2
        } yield (_1, _2)
    }
  }

  "compose" when {
    "used on two applicatives" should {
      val compose = Monad.option.compose(Applicative.validation[String])

      "obey identity law" in {
        checkIdentityLaw(
          applicative = compose
        )(
          gen = optionValidationGen(Gen.long)
        )
      }

      "obey associativity law" in {
        checkAssociativityLaw(
          applicative = compose
        )(
          gen1 = optionValidationGen(Gen.long),
          gen2 = optionValidationGen(Gen.asciiChar),
          gen3 = optionValidationGen(Gen.double)
        )
      }

      "obey naturality law" in {
        checkNaturalityLaw(
          applicative = compose
        )(
          gen1 = optionValidationGen(Gen.long),
          gen2 = optionValidationGen(Gen.asciiChar)
        )(
          genF = Gen.function1[Long, String](Gen.numStr),
          genG = Gen.function1[Char, Int](Gen.size)
        )
      }

      def optionValidationGen[T](gen: Gen[T]): Gen[Option[Validation[String, T]]] =
        Gen.option(validationGen(gen, Gen.asciiStr))
    }
  }

  def validationGen[A, E](successValue: Gen[A], failureValues: Gen[E]): Gen[Validation[E, A]] = {
    val successGen = successValue.map(Success(_))
    val failureGen = Gen.nonEmptyListOf(failureValues).map {
      case head :: tail => Failure(head, tail: _*)
    }
    val eitherGen = Gen.either(failureGen, successGen)
    eitherGen.map {
      case Right(success) => success
      case Left(failure) => failure
    }
  }

  def checkIdentityLaw[F[_], A](applicative: Applicative[F])(gen: Gen[F[A]]): Assertion =
    forAll(gen) { value =>
      applicative.map(value)(identity) should be(value)
    }

  def checkAssociativityLaw[F[_], A, B, C](applicative: Applicative[F])(gen1: Gen[F[A]], gen2: Gen[F[B]], gen3: Gen[F[C]]): Assertion = {
    import applicative.{map, product}

    val gen = for {
      _1 <- gen1
      _2 <- gen2
      _3 <- gen3
    } yield (_1, _2, _3)

    forAll(gen) {
      case (_1, _2, _3) =>
        val left = product(product(_1, _2), _3)
        val right = map(product(_1, product(_2, _3))) {
          case (a, (b, c)) => ((a, b), c) // Transform to match the structure of left
        }

        left should be(right)
    }
  }

  def checkNaturalityLaw[F[_], A, A1, B, B1](applicative: Applicative[F])(gen1: Gen[F[A]], gen2: Gen[F[B]])(genF: Gen[A => A1], genG: Gen[B => B1]): Assertion = {
    import applicative.{product, map, map2}

    val gen = for {
      _1 <- gen1
      _2 <- gen2
      f <- genF
      g <- genG
    } yield (_1, _2, f, g)

    forAll(gen) {
      case (_1, _2, f, g) =>
        map2(_1, _2)((a, b) => (f(a), g(b))) should be(product(map(_1)(f), map(_2)(g)))
    }
  }
}
