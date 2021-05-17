package chapter6

import chapter6.mystate.MyState

sealed trait Input

case object Coin extends Input

case object TurnKnob extends Input

case class Machine private[chapter6](locked: Boolean, candies: Int, coins: Int) {

  def insertCoin(): Machine =
    if (locked && candies > 0)
      copy(locked = false, coins = coins + 1)
    else
      this

  def turnKnob(): Machine =
    if (!locked && candies > 0)
      copy(locked = true, candies = candies - 1)
    else
      this
}

object Machine {

  def apply(candies: Int, coins: Int): Machine = {
    require(candies >= 0 && coins >= 0)
    Machine(locked = true, candies = candies, coins = coins)
  }

  def simulateMachine(inputs: List[Input]): MyState[Machine, (Int, Int)] = {
    machine =>
      val resultMachine = inputs.foldLeft(machine) {
        case (machine, Coin) => machine.insertCoin()
        case (machine, TurnKnob) => machine.turnKnob()
      }
      ((resultMachine.candies, resultMachine.coins), resultMachine)
  }
}
