package chapter8

import chapter6.Rng
import chapter8.Prop.TestCases

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class Prop(run: (TestCases, Rng) => Result) {
  def &&(prop: Prop): Prop = Prop { (testCases, rng) =>
    this.run(testCases, rng) match {
      case Passed => prop.run(testCases, rng)
      case falsified => falsified
    }
  }

  def ||(prop: Prop): Prop = Prop { (testCases, rng) =>
    this.run(testCases, rng) match {
      case _: Falsified => prop.run(testCases, rng)
      case passed => passed
    }
  }
}

object Prop {
  type TestCases = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    Prop(ForAllPropChecker(gen, f, _).check(_))

  private case class ForAllPropChecker[A](gen: Gen[A], f: A => Boolean, testCases: TestCases) {

    def check(rng: Rng): Result =
      check(casesLeft = testCases, rng)

    @tailrec
    private def check(casesLeft: Int, rng: Rng): Result =
      if (casesLeft <= 0) Passed
      else {
        val (generatedValue, rng2) = gen.sample.run(rng)
        val result = Try(f(generatedValue))
        result match {
          case Success(true) => check(casesLeft - 1, rng2)
          case Success(false) => Falsified(failureMessage(generatedValue), successes = testCases - casesLeft)
          case Failure(exception) => Falsified(failureMessage(generatedValue, exception), successes = testCases - casesLeft)
        }
      }

    private def failureMessage(generatedValue: A) =
      s"Property was falsified for value: $generatedValue"

    private def failureMessage(generatedValue: A, exception: Throwable) =
      s"""Check failed with exception for value: $generatedValue
         |${exception.getStackTrace.mkString("\n")}
         |""".stripMargin
  }
}

sealed trait Result

case object Passed extends Result

case class Falsified(failure: String, successes: Int) extends Result