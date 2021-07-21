package chapter8

import chapter6.{Rng, State}

case class Gen[A](sample: State[Rng, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val state = sample.flatMap(f(_).sample)
    Gen(state)
  }

  def listOfN(size: Int): Gen[List[A]] =
    listOfN(Gen.unit(size))

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap { size =>
      val states = List.fill(size)(sample)
      val state = State.sequence(states)
      Gen(state)
    }
  }

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def int: Gen[Int] =
    Gen(State(Rng.int))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val state = State(Rng.nonNegativeInt).map(toRange(_, start, stopExclusive))
    Gen(state)
  }

  private[chapter8] def toRange(i: Int, start: Int, stopExclusive: Int) = {
    (i % (stopExclusive - start)) + start
  }

  def unit[A](a: => A): Gen[A] = {
    val state = State(Rng.unit(a))
    Gen(state)
  }

  def boolean: Gen[Boolean] = {
    val state = State(Rng.double).map(_ < 0.5)
    Gen(state)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    weighted((g1, 1), (g2, 1))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val totalWeight = g1._2.abs + g2._2.abs
    val g1Probability = g1._2.abs / totalWeight
    val state = State(Rng.double).flatMap { randomDouble =>
      val chosenGen = if (randomDouble < g1Probability) g1 else g2
      chosenGen._1.sample
    }
    Gen(state)
  }
}
