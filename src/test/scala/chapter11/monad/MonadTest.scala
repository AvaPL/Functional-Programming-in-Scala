package chapter11.monad

import chapter6.{Rng, State}
import chapter7.Nonblocking.Par
import chapter8.Gen
import chapter9.Parser
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.util.concurrent.Executors
import scala.reflect.ClassTag

class MonadTest extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "filterM" when {
    "used with List monad" should {
      "return Nil when no booleans are supplied" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.list.filterM(list)(_ => Nil)

        result should be(Nil)
      }

      "behave like standard filter for only one boolean" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.list.filterM(list)(i => List(i % 2 == 0))

        result should be(List(List(2, 4)))
      }

      "return Nil when certain values return None" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.list.filterM(list)(i => Option.when(i % 2 != 0)(i % 3 != 0).toList)

        result should be(Nil)
      }

      // When list is bigger than 1 it becomes too complex to understand or use.
      // Leaving it without tests.
    }

    "used with Option monad" should {
      "return None when None boolean is always supplied" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.option.filterM(list)(_ => None)

        result should be(None)
      }

      "behave like standard filter for only one boolean" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.option.filterM(list)(i => Some(i % 2 == 0))

        result should be(Some(List(2, 4)))
      }

      "return None when certain values return None" in {
        val list = List(1, 2, 3, 4, 5)

        val result = Monad.option.filterM(list)(i => Option.when(i % 2 != 0)(i % 3 != 0))

        result should be(None)
      }
    }
  }

  "compose" when {
    "used on two functions" should {
      "create a composed function" in {
        val function1 = (i: Int) => Some(i.toString)
        val function2 = (s: String) => Some(s * 3)
        val composed = Monad.option.compose(function1, function2)

        val result = composed(5)

        result should be(Some("555"))
      }
    }
  }

  checkAssociativeLaw(Monad.option)((i: Int) => Some(i.toString))(_.headOption)(_.toString.toIntOption)

  checkAssociativeLaw(Monad.list)((i: Int) => List(i.toString))(s => List.fill(5)(s))(s => s.split("").toList)

  checkAssociativeLaw(Monad.lazyList)((i: Int) => LazyList(i.toString))(s => LazyList.fill(5)(s))(s => LazyList.from(s.split("")))

  def checkAssociativeLaw[F[_], A, B, C, D, E]
  (monad: Monad[F])
  (f: A => F[B])
  (g: B => F[C])
  (h: C => F[D])
  (implicit monadType: ClassTag[F[_]], arbitrary: Arbitrary[A]): Unit = {
    s"Monad[$monadType]" should {
      "obey associative law" in {
        import monad.compose
        forAll { a: A =>
          val left = compose(compose(f, g), h)(a)
          val right = compose(f, compose(g, h))(a)

          left should be(right)
        }
      }
    }
  }

  "gen" should {
    import Monad.gen.{compose, unit}
    val rng = Rng.Simple(0)

    "obey associative law" in {
      val f = (i: Int) => Gen.unit(i.toString)
      val g = (s: String) => Gen.union(Gen.unit(s), Gen.unit(255.toString))
      val h = (s: String) => Gen.choose(s.toInt, s.toInt + 10)

      forAll { i: Int =>
        val leftGen = compose(compose(f, g), h)(i)
        val rightGen = compose(f, compose(g, h))(i)

        val left = leftGen.sample.run(rng)._1
        val right = rightGen.sample.run(rng)._1

        left should be(right)
      }
    }

    "obey identity laws" in {
      forAll { i: Int =>
        val f = (i: Int) => Gen.unit(i.toString)
        val leftGen = compose(f, unit[String])(i)
        val rightGen = compose(unit[Int], f)(i)

        val fResult = f(i).sample.run(rng)._1
        val left = leftGen.sample.run(rng)._1
        val right = rightGen.sample.run(rng)._1

        left should be(fResult)
        right should be(fResult)
      }
    }
  }

  "par" should {
    import Monad.par.{compose, unit}
    "obey associative law" in {
      val f = (i: Int) => Par.unit(i.toString)
      val g = (s: String) => Par.lazyUnit(s * 3)
      val h = (s: String) => Par.delay(s.split(""))

      forAll { i: Int =>
        val leftPar = compose(compose(f, g), h)(i)
        val rightPar = compose(f, compose(g, h))(i)
        val executorService = Executors.newFixedThreadPool(2)

        val left = Par.run(executorService)(leftPar)
        val right = Par.run(executorService)(rightPar)

        left should be(right)
      }
    }

    "obey identity laws" in {
      forAll { i: Int =>
        val f = (i: Int) => Par.unit(i.toString)
        val leftPar = compose(f, unit[String])(i)
        val rightPar = compose(unit[Int], f)(i)

        val executorService = Executors.newFixedThreadPool(2)
        val fResult = Par.run(executorService)(f(i))
        val left = Par.run(executorService)(leftPar)
        val right = Par.run(executorService)(rightPar)

        left should be(fResult)
        right should be(fResult)
      }
    }
  }

  "parser" should {
    import Monad.parser.{compose, unit}
    "obey associative law" in {
      val f = (i: Int) => Parser.string(i.toString)
      val g = (s: String) => Parser.regex(s.r)
      val h = (s: String) => Parser.char(s.head)

      forAll { (i: Int, s: String) =>
        val leftParser = compose(compose(f, g), h)(i)
        val rightParser = compose(f, compose(g, h))(i)

        val left = leftParser.parse(s)
        val right = rightParser.parse(s)

        left should be(right)
      }
    }

    "obey identity laws" in {
      forAll { (i: Int, s: String) =>
        val f = (i: Int) => Parser.string(i.toString)
        val leftParser = compose(f, unit[String])(i)
        val rightParser = compose(unit[Int], f)(i)

        val fResult = f(i).parse(s)
        val left = leftParser.parse(s)
        val right = rightParser.parse(s)

        left should be(fResult)
        right should be(fResult)
      }
    }
  }

  "state" should {
    val monad = Monad.state[Rng]
    import monad.{compose, unit}
    val rng = Rng.Simple(0)

    "obey associative law" in {
      val f = (i: Int) => State.unit[Rng, Int](i)
      val g = (i: Int) => State.sequence[Rng, String](List.fill(i.min(100))(State.unit[Rng, String](i.toString)))
      val h = (s: List[String]) => State.unit[Rng, String](s.mkString(", "))

      forAll { i: Int =>
        val leftState = compose(compose(f, g), h)(i)
        val rightState = compose(f, compose(g, h))(i)

        val left = leftState.run(rng)._1
        val right = rightState.run(rng)._1

        left should be(right)
      }
    }

    "obey identity laws" in {
      forAll { i: Int =>
        val f = (i: Int) => State.unit[Rng, String](i.toString)
        val leftState = compose(f, unit[String])(i)
        val rightState = compose(unit[Int], f)(i)

        val fResult = f(i).run(rng)._1
        val left = leftState.run(rng)._1
        val right = rightState.run(rng)._1

        left should be(fResult)
        right should be(fResult)
      }
    }
  }

  "flatMapViaCompose" should {
    "give same results as flatMap" in {
      forAll { i: Int =>
        val list = List(i, i * 2, i + 5)
        val f = (i: Int) => i.toString.toList

        Monad.list.flatMapViaCompose(list)(f) should be(Monad.list.flatMap(list)(f))
      }
    }
  }

  checkIdentityLaws(Monad.option)((i: Int) => Some(i.toString))

  checkIdentityLaws(Monad.list)((i: Int) => List(i.toString))

  checkIdentityLaws(Monad.lazyList)((i: Int) => LazyList(i.toString))

  def checkIdentityLaws[F[_], A, B]
  (monad: Monad[F])
  (f: A => F[B])
  (implicit monadType: ClassTag[F[_]], arbitrary: Arbitrary[A]): Unit = {
    s"Monad[$monadType]" should {
      "obey identity laws" in {
        import monad.{compose, unit}
        forAll { a: A =>
          compose(f, unit[B])(a) should be(f(a))
          compose(unit[A], f)(a) should be(f(a))
        }
      }
    }
  }

  "join" when {
    "given nested types" should {
      "unwrap them" in {
        val nested = List(List(1, 2), List(3, 4))

        Monad.list.join(nested) should be(List(1, 2, 3, 4))
      }
    }
  }

  "flatMapViaJoinAndMap" should {
    "give same results as flatMap" in {
      forAll { i: Int =>
        val list = List(i, i * 2, i + 5)
        val f = (i: Int) => i.toString.toList

        Monad.list.flatMapViaJoinAndMap(list)(f) should be(Monad.list.flatMap(list)(f))
      }
    }
  }

  "composeViaJoinAndMap" should {
    "give same results as compose" in {
      val function1 = (i: Int) => Some(i.toString)
      val function2 = (s: String) => Some(s * 3)
      val input = 5

      val left = Monad.option.composeViaJoinAndMap(function1, function2)(input)
      val right = Monad.option.compose(function1, function2)(input)

      left should be(right)
    }
  }
}