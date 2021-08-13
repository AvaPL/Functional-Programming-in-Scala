package chapter10.foldable

import chapter3.Branch
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FoldableTest extends AnyWordSpec with Matchers {
  "list" when {
    "foldLeft is called" should {
      "concatenate from left" in {
        val list = List("a", "b", "c")

        val result = Foldable.list.foldLeft(list)("")(_ + _)

        result should be("abc")
      }
    }

    "foldRight is called" should {
      "concatenate from right" in {
        val list = List("a", "b", "c")

        val result = Foldable.list.foldRight(list)("")(_ + _)

        result should be("cba")
      }
    }
  }

  "indexedSeq" when {
    "foldLeft is called" should {
      "concatenate from left" in {
        val seq = IndexedSeq("a", "b", "c")

        val result = Foldable.indexedSeq.foldLeft(seq)("")(_ + _)

        result should be("abc")
      }
    }

    "foldRight is called" should {
      "concatenate from right" in {
        val seq = IndexedSeq("a", "b", "c")

        val result = Foldable.indexedSeq.foldRight(seq)("")(_ + _)

        result should be("cba")
      }
    }
  }

  "lazyList" when {
    "foldLeft is called" should {
      "concatenate from left" in {
        val list = LazyList("a", "b", "c")

        val result = Foldable.lazyList.foldLeft(list)("")(_ + _)

        result should be("abc")
      }
    }

    "foldRight is called" should {
      "concatenate from right" in {
        val list = LazyList("a", "b", "c")

        val result = Foldable.lazyList.foldRight(list)("")(_ + _)

        result should be("cba")
      }
    }
  }

  "tree" when {
    "foldLeft is called" should {
      "concatenate from left" in {
        //  /\
        // a /\
        //  /\ d
        // b  c
        val tree = Branch("a", Branch(Branch("b", "c"), "d"))

        val result = Foldable.tree.foldLeft(tree)("")(_ + _)

        result should be("abcd")
      }
    }

    "foldRight is called" should {
      "concatenate from right" in {
        //  /\
        // a /\
        //  /\ d
        // b  c
        val tree = Branch("a", Branch(Branch("b", "c"), "d"))

        val result = Foldable.tree.foldRight(tree)("")(_ + _)

        result should be("dcba")
      }
    }
  }

  "option" when {
    "foldLeft is called" should {
      "return default value when None" in {
        val option = Option.empty[Int]

        val result = Foldable.option.foldLeft(option)(5)(identity)

        result should be(5)
      }

      "return value inside Some" in {
        val option = Some(10)

        val result = Foldable.option.foldLeft(option)(5)(identity)

        result should be(10)
      }

      def identity[A](x: A, y: A) = y
    }

    "foldRight is called" should {
      "return default value when None" in {
        val option = Option.empty[Int]

        val result = Foldable.option.foldRight(option)(5)(identity)

        result should be(5)
      }

      "return value inside Some" in {
        val option = Some(10)

        val result = Foldable.option.foldRight(option)(5)(identity)

        result should be(10)
      }

      def identity[A](x: A, y: A) = x
    }
  }
}
