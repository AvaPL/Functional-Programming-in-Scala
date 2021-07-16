package chapter8

import chapter6.Rng
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SGenTest extends AnyWordSpec with Matchers {
  "listOf" when {
    "given a Gen" should {
      "produce 2 distinct lists" in {
        val List(first, second) = generateNListsOfSizeM(2, 5)

        first should not be second
      }

      "produce 100 lists of size 5" in {
        val lists = generateNListsOfSizeM(100, 5)

        val sizes = lists.map(_.size)
        all(sizes) should be(5)
      }
    }

    def generateNListsOfSizeM(n: Int, m: Int) = {
      val gen = Gen.int
      val rng = Rng.Simple(0)
      val listOfSizeM = SGen.listOf(gen).forSize(m)
      listOfSizeM.listOfN(n).sample.run(rng)._1
    }
  }
}
