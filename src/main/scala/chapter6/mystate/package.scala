package chapter6

package object mystate {

  type MyState[S, +A] = S => (A, S)

  implicit class MyStateOps[S, +A](val run: MyState[S, A]) extends AnyVal {
    def map[B](f: A => B): MyState[S, B] =
      this.flatMap(a => MyStateOps.unit(f(a)))

    def flatMap[B](f: A => MyState[S, B]): MyState[S, B] =
      { s =>
        val (a, s2) = this.run(s)
        f(a).run(s2)
      }
  }

  object MyStateOps {
    def unit[S, A](a: A): MyState[S, A] =
      s => (a, s)

    def map2[S, A, B, C](stateA: MyState[S, A], stateB: MyState[S, B])(f: (A, B) => C): MyState[S, C] =
      stateA.flatMap(a => stateB.map(b => f(a, b)))

    def sequence[S, A](states: List[MyState[S, A]]): MyState[S, List[A]] =
      states.foldRight(unit[S, List[A]](Nil))(map2(_, _)(_ :: _))
  }
}
