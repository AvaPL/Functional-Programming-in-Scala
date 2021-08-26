package chapter6

import chapter6.State.unit

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    this.flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = this.run(s)
      f(a).run(s2)
    }
}

object State {
  type Rand[A] = State[Rng, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def map2[S, A, B, C](stateA: State[S, A], stateB: State[S, B])(f: (A, B) => C): State[S, C] =
    stateA.flatMap(a => stateB.map(b => f(a, b)))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(unit[S, List[A]](Nil))(map2(_, _)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}