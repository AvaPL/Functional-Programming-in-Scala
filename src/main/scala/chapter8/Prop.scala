package chapter8

import chapter5.Stream
import chapter6.Rng
import chapter8.Prop._

import scala.annotation.tailrec
import scala.math.abs
import scala.util.{Failure, Success, Try}

case class Prop(run: (MaxSize, TestCases, Rng) => Result) {
  def run(testCases: TestCases, rng: Rng): Result =
    run(Int.MaxValue, testCases, rng)

  def check(maxSize: MaxSize = 100, testCases: TestCases = 100, rng: Rng = Rng.Simple(System.currentTimeMillis())): Unit =
    run(maxSize, testCases, rng) match {
      case Passed => println(s"Passed after $testCases test cases.")
      case Falsified(failure, successes) => Console.err.println(s"Falsified after $successes successful test cases:\n$failure")
    }

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
    Prop((_, testCases, rng) => ForAllPropChecker(gen, f, testCases).check(rng))

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
      val gens = sizes.map(sgen.forSize)
      val props = gens.map(forAll(_)(f))
      LazyList.from(props).zip(cases).map {
        case (prop, testCases) => prop.run(testCases, rng)
      }.collectFirst {
        case falsified: Falsified => falsified
      }.getOrElse(Passed)
    }

  private[chapter8] def calculateSizes(maxSize: MaxSize) = {
    // 0, 1, 2, 4, 8, 16, ...
    Option.when(maxSize >= 0)(0) ++: Stream.iterate(1)(_ * 2).takeWhile(_ <= maxSize).toList
  }

  private[chapter8] def calculateTestCases(testCases: TestCases, sizesCount: Int) =
    (testCases, sizesCount) match {
      case (_, sizesCount) if sizesCount < 1 => Nil
      case (testCases, sizesCount) if testCases <= sizesCount => List.fill(testCases)(1).padTo(sizesCount, 0)
      case (testCases, sizesCount) if sizesCount == 1 => List(testCases)
      case (testCases, sizesCount) => linearTestCases(testCases, sizesCount)
    }

  private def linearTestCases(testCases: TestCases, sizesCount: MaxSize) = {
    // From 1 to x distributed linearly so the total number of cases equals testCases
    val step = 2.0 * (testCases - sizesCount) / (sizesCount * (sizesCount - 1))
    val cases = Stream.fromViaUnfold(1, step).map(_.toInt).take(sizesCount).toArray
    val casesLeftToDistribute = testCases - cases.sum
    for {i <- 0 until casesLeftToDistribute} {
      val index = abs(cases.length - 1 - i) % cases.length
      cases(index) += 1
    }
    cases.toList
  }
}
