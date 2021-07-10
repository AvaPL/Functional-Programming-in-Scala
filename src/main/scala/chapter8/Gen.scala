package chapter8

import chapter6.{Rng, State}

case class Gen[A](sample: State[Rng, A])

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val state = State[Rng, Int](_.nextInt).map(toRange(_, start, stopExclusive))
    Gen(state)
  }

  private[chapter8] def toRange(i: Int, start: Int, stopExclusive: Int) = {
    (i % (stopExclusive - start)) + start
  }
}
