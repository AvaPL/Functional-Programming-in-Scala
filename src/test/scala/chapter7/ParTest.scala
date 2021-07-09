package chapter7

import chapter7.Par.{Par, choice}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.TimeoutException

class ParTest extends AnyWordSpec with Matchers {
  "map2 with timeout handling" when {
    "timeout is not exceeded" should {
      "return result of mapping two values" in {
        val computation1 = computation(1, 100, TimeUnit.MILLISECONDS)
        val computation2 = computation(2, 100, TimeUnit.MILLISECONDS)

        val result = execute(computation1, computation2, 1000, TimeUnit.MILLISECONDS)
        result should be(3)
      }
    }

    "timeout is exceeded by second computation" should {
      "return result of mapping two values" in {
        val computation1 = computation(1, 100, TimeUnit.MILLISECONDS)
        val computation2 = computation(2, 200, TimeUnit.MILLISECONDS)

        a[TimeoutException] should be thrownBy execute(computation1, computation2, 150, TimeUnit.MILLISECONDS)
      }
    }

    "timeout is exceeded by first computation" should {
      "return result of mapping two values" in {
        val computation1 = computation(1, 200, TimeUnit.MILLISECONDS)
        val computation2 = computation(2, 100, TimeUnit.MILLISECONDS)

        a[TimeoutException] should be thrownBy execute(computation1, computation2, 150, TimeUnit.MILLISECONDS)
      }
    }

    def computation(result: Int, executionTime: Long, timeUnit: TimeUnit): Par[Int] =
      _.submit {
        () => {
          timeUnit.sleep(executionTime)
          result
        }
      }

    def execute(a: Par[Int], b: Par[Int], timeout: Long, timeUnit: TimeUnit) = {
      val executorService = Executors.newFixedThreadPool(2)
      Par.map2(a, b, timeout, timeUnit)(_ + _)(executorService).get
    }
  }

  "asyncF" when {
    "given identity function" should {
      "return the input value" in {
        execute(1)(identity) should be(1)
      }
    }

    "given string conversion function" should {
      "return an integer converted to string" in {
        execute(1)(_.toString) should be("1")
      }
    }

    def execute[A, B](a: A)(f: A => B) = {
      val executorService = Executors.newFixedThreadPool(2)
      Par.asyncF(f)(a)(executorService).get()
    }
  }

  "sequence" when {
    "given empty list" should {
      "return empty list" in {
        execute(Nil) shouldBe empty
      }
    }

    "given list with one element" should {
      "return list with one element" in {
        val list = List(5)

        execute(list) should be(list)
      }
    }

    "given list with multiple elements" should {
      "return list with elements in correct order" in {
        val list = List(1, 5, 3)

        execute(list) should be(list)
      }
    }

    def execute[A](list: List[A]) = {
      val listPar = list.map(Par.unit)
      val executorService = Executors.newFixedThreadPool(2)
      Par.sequence(listPar)(executorService).get
    }
  }

  "parFilter" when {
    "given empty list" should {
      "return empty list for always false predicate" in {
        execute(Nil)(_ => false) shouldBe empty
      }

      "return empty list for always true predicate" in {
        execute(Nil)(_ => true) shouldBe empty
      }
    }

    "given list with one element" should {
      "return empty list for always false predicate" in {
        execute(List(5))(_ => false) shouldBe empty
      }

      "return initial list for always true predicate" in {
        val list = List(5)

        execute(list)(_ => true) should be(list)
      }
    }

    "given list with multiple elements" should {
      "return empty list for always false predicate" in {
        execute(List(1, 5, 3))(_ => false) shouldBe empty
      }

      "return initial list for always true predicate" in {
        val list = List(1, 5, 3)

        execute(list)(_ => true) should be(list)
      }

      "filter matching elements from predicate" in {
        execute(List(1, 5, 3, 5, 2))(_ != 5) should be(List(1, 3, 2))
      }
    }

    def execute[A](list: List[A])(f: A => Boolean) = {
      val executorService = Executors.newFixedThreadPool(2)
      Par.parFilter(list)(f)(executorService).get
    }
  }

  "choice" when {
    "given a condition that returns true" should {
      "return true Par" in {
        val condition = Par.unit(true)
        val truePar = Par.unit(123)
        val falsePar = Par.unit(321)

        val result = execute(condition)(truePar, falsePar)

        result should be(123)
      }
    }

    "given a condition that returns false" should {
      "return false Par" in {
        val condition = Par.unit(false)
        val truePar = Par.unit(123)
        val falsePar = Par.unit(321)

        val result = execute(condition)(truePar, falsePar)

        result should be(321)
      }
    }

    def execute[A](condition: Par[Boolean])(truePar: Par[A], falsePar: Par[A]) = {
      val executorService = Executors.newFixedThreadPool(2)
      Par.choice(condition)(truePar, falsePar)(executorService).get
    }
  }

  "flatMap" when {
    "given a Par and dependent Par function" should {
      "return a non nested Par" in {
        val parInt = Par.unit(5)

        def parString(int: Int) = Par.unit(int.toString)

        val executorService = Executors.newFixedThreadPool(2)
        val result = Par.flatMap(parInt)(parString)(executorService).get

        result should be("5")
      }
    }
  }

  "flatten" when {
    "given a nested Par" should {
      "unnest Par" in {
        val nested = Par.unit(Par.unit(5))

        val executorService = Executors.newFixedThreadPool(2)
        val result = Par.flatten(nested)(executorService).get

        result should be(5)
      }
    }
  }
}
