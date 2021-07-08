package chapter7

import chapter7.Par.Par
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
}
