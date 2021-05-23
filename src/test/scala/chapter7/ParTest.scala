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
  }

  private def computation(result: Int, executionTime: Long, timeUnit: TimeUnit): Par[Int] =
    _.submit {
      () => {
        timeUnit.sleep(executionTime)
        result
      }
    }

  private def execute(a: Par[Int], b: Par[Int], timeout: Long, timeUnit: TimeUnit) = {
    val executorService = Executors.newFixedThreadPool(4)
    Par.map2(a, b, timeout, timeUnit)(_ + _)(executorService).get
  }
}
