package chapter8

import chapter5.Stream
import chapter6.Rng
import chapter8.Prop._

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

case class Prop(run: (MaxSize, TestCases, Rng) => Result) {
  def &&(prop: Prop): Prop = Prop { (maxSize, testCases, rng) =>
    this.run(maxSize, testCases, rng) match {
      case Passed => prop.run(maxSize, testCases, rng)
      case falsified => falsified
    }
  }

  def ||(prop: Prop): Prop = Prop { (maxSize, testCases, rng) =>
    this.run(maxSize, testCases, rng) match {
      case _: Falsified => prop.run(maxSize, testCases, rng)
      case passed => passed
    }
  }
}

object Prop {
  type MaxSize = Int

  type TestCases = Int

  sealed trait Result

  case object Passed extends Result

  case class Falsified(failure: String, successes: Int) extends Result

  def apply(run: (TestCases, Rng) => Result): Prop =
    Prop((_, testCases, rng) => run(testCases, rng))

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

  def forAll[A](sgen: SGen[A])(f: A => Boolean): Prop =
    Prop { (maxSize, testCases, rng) =>
      val sizes = calculateSizes(maxSize)
      val cases = calculateTestCases(testCases, sizes.size)
      // TODO: Use cases and sizes
      val gens = sizes.map(sgen.forSize)
      val prop = gens.map(forAll(_)(f)).reduce(_ && _)
      prop.run(maxSize, testCases, rng)
    }

  private def calculateSizes(maxSize: MaxSize) = {
    // 0, 1, 2, 4, 8, 16, ...
    // TODO: Add tests
    0 :: LazyList.iterate(1)(_ * 2).takeWhile(_ < maxSize).toList
  }

  private def calculateTestCases(testCases: TestCases, sizesCount: Int) = {
    // From 1 to x distributed linearly so the total number of cases equals testCases
    // TODO: Handle sizesCount <= 1 and testCases < 0
    val x = testCases / (sizesCount - 1) + 1
    val step = (x - 1) / (sizesCount - 1)
    val cases = (1 to x by step).toArray // TODO: Use BigDecimal range?
    val casesToRandomlyDistribute = testCases - cases.sum
    for {_ <- 0 to casesToRandomlyDistribute} {
      val randomIndex = Random.nextInt(cases.length)
      cases(randomIndex) += 1
    }
    // TODO: Ensure that result has size of sizesCount
    cases.toList
  }
}
