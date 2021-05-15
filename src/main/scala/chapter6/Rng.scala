package chapter6

import scala.annotation.tailrec
import scala.math.abs

trait Rng {
  def nextInt: (Int, Rng)
}

object Rng {

  case class Simple(seed: Long) extends Rng {
    def nextInt: (Int, Rng) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = Rng => (A, Rng)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: Rng): (Int, Rng) = {
    val (value, rng2) = rng.nextInt
    (abs(value % Int.MaxValue), rng2)
  }

  def double(rng: Rng): (Double, Rng) =
    map(nonNegativeInt)(_ / (Int.MaxValue + 1.0))(rng)

  def intDouble(rng: Rng): ((Int, Double), Rng) = {
    val (intValue, rng2) = rng.nextInt
    val (doubleValue, rng3) = double(rng2)
    ((intValue, doubleValue), rng3)
  }

  def doubleInt(rng: Rng): ((Double, Int), Rng) = intDouble(rng) match {
    case ((intValue, doubleValue), rng2) => ((doubleValue, intValue), rng2)
  }

  def double3(rng: Rng): ((Double, Double, Double), Rng) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: Rng): (List[Int], Rng) = {
    @tailrec
    def loop(count: Int, rng: Rng, result: List[Int] = Nil): (List[Int], Rng) = rng.nextInt match {
      case (value, rng) if count > 0 => loop(count - 1, rng, value :: result)
      case _ => (result.reverse, rng)
    }

    loop(count, rng)
  }

  def map2[A, B, C](randA: Rand[A], randB: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, rng2) = randA(rng)
      val (b, rng3) = randB(rng2)
      (f(a, b), rng3)
  }

  def sequence[A](rands: List[Rand[A]]): Rand[List[A]] =
    rands.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  def intsViaSequence(count: Int)(rng: Rng): (List[Int], Rng) =
    sequence(List.fill(count)((rng: Rng) => rng.nextInt))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}
