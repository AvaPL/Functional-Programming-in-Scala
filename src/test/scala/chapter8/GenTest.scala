package chapter8

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GenTest extends AnyWordSpec with Matchers {
  "toRange" when {
    "given number outside the range" should {
      "convert value equal to stopExclusive to value in range" in {
        val start = 1
        val value = 10
        val stopExclusive = value

        val result = Gen.toRange(value, start, stopExclusive)

        result should (be >= start and be < stopExclusive)
      }

      "convert value of start - 1 to value in range" in {
        val start = 1
        val value = start - 1
        val stopExclusive = 10

        val result = Gen.toRange(value, start, stopExclusive)

        result should (be >= start and be < stopExclusive)
      }
    }
  }
}
