package chapter6

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MachineTest extends AnyWordSpec with Matchers {

  "apply" when {
    "given negative candies" should {
      "throw an IllegalArgumentException" in {
        an[IllegalArgumentException] should be thrownBy Machine(candies = -1, coins = 1)
      }
    }

    "given negative coins" should {
      "throw an IllegalArgumentException" in {
        an[IllegalArgumentException] should be thrownBy Machine(coins = -1, candies = 1)
      }
    }

    "given 0 candies" should {
      "not throw any exception" in {
        noException should be thrownBy Machine(candies = 0, coins = 1)
      }
    }

    "given 0 coins" should {
      "not throw any exception" in {
        noException should be thrownBy Machine(coins = 0, candies = 1)
      }
    }

    "given positive candies and coins" should {
      "not throw any exception" in {
        noException should be thrownBy Machine(candies = 5, coins = 2)
      }
    }

    "machine is created" should {
      "start in locked state" in {
        Machine(candies = 5, coins = 2).locked should be(true)
      }
    }
  }

  "insertCoin" when {
    "machine is locked" should {
      "unlock the machine and increment coins" in {
        val lockedMachine = Machine(candies = 5, coins = 0)

        val result = lockedMachine.insertCoin()

        result.locked should be(false)
        result.coins should be(lockedMachine.coins + 1)
      }
    }

    "machine is unlocked" should {
      "do nothing" in {
        val unlockedMachine = Machine(locked = false, candies = 5, coins = 0)

        unlockedMachine.insertCoin() should be(unlockedMachine)
      }
    }

    "machine is unlocked and has no candies" should {
      "do nothing" in {
        val machineWithNoCandies = Machine(locked = false, candies = 0, coins = 0)

        machineWithNoCandies.insertCoin() should be(machineWithNoCandies)
      }
    }
  }

  "turnKnob" when {
    "machine is locked" should {
      "do nothing" in {
        val lockedMachine = Machine(candies = 5, coins = 0)

        lockedMachine.turnKnob() should be(lockedMachine)
      }
    }

    "machine is unlocked" should {
      "lock the machine and decrement candies" in {
        val unlockedMachine = Machine(locked = false, candies = 5, coins = 0)

        val result = unlockedMachine.turnKnob()

        result.locked should be(true)
        result.candies should be(unlockedMachine.candies - 1)
      }
    }

    "machine is unlocked and has no candies" should {
      "do nothing" in {
        val machineWithNoCandies = Machine(locked = false, candies = 0, coins = 0)

        machineWithNoCandies.turnKnob() should be(machineWithNoCandies)
      }
    }
  }

  "simulateMachine" when {
    "called without any inputs" should {
      "return initial machine state" in {
        val machine = Machine(candies = 5, coins = 5)

        Machine.simulateMachine(Nil)(machine) should be((5, 5), machine)
      }
    }

    "called with one input" should {
      "return unlocked machine when coin is inserted" in {
        val machine = Machine(candies = 5, coins = 5)
        val inputs = List(Coin)

        Machine.simulateMachine(inputs)(machine) should be((5, 6), Machine(locked = false, candies = 5, coins = 6))
      }
    }

    "called with multiple inputs" should {
      "execute inputs in order" in {
        val machine = Machine(candies = 5, coins = 5)
        val inputs = List(Coin, TurnKnob, Coin, TurnKnob)

        Machine.simulateMachine(inputs)(machine) should be((3, 7), Machine(locked = true, candies = 3, coins = 7))
      }
    }
  }
}
