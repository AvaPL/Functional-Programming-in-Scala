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

  def unit[A](a: => A): Gen[A] = {
    val state = State[Rng, A](Rng.unit(a))
    Gen(state)
  }

  def boolean: Gen[Boolean] = {
    val state = State[Rng, Double](Rng.double).map(_ < 0.5)
    Gen(state)
  }

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = {
    val states = List.fill(n)(gen.sample)
    val state = State.sequence(states)
    Gen(state)
  }
}
