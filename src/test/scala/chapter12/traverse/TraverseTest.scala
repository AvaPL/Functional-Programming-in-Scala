package chapter12.traverse

import chapter10.monoid.Monoid
import chapter12.monad.Monad
import chapter3.{Branch, Leaf}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TraverseTest extends AnyWordSpec with Matchers {
  implicit val optionApplicative: Monad[Option] = Monad.option

  "list" when {
    "traverse is used on list of options" should {
      "return Some when no elements in list are None" in {
        val list = List(Some(1), Some(2), Some(3))

        val result = Traverse.list.traverse(list)(_.map(_.toString))

        result should contain(List("1", "2", "3"))
      }

      "return None when at least one element in list is None" in {
        val list = List(Some(1), Some(2), Some(3))

        val result = Traverse.list.traverse(list)(_.filter(_ % 2 != 0).map(_.toString))

        result shouldBe empty
      }
    }
  }

  "option" when {
    "traverse is used on option of option" should {
      "return Some when inner element is Some" in {
        val option = Some(Some(1))

        val result = Traverse.option.traverse(option)(_.map(_.toString))

        result should contain(Some("1"))
      }

      "return None when at least one element in list is None" in {
        val option = Some(Option.empty[Int])

        val result = Traverse.option.traverse(option)(_.map(_.toString))

        result shouldBe empty
      }
    }
  }

  "tree" when {
    "traverse is used on tree of options" should {
      "return Some when no elements in tree are None" in {
        val tree = Branch(Branch(Some(1), Some(2)), Leaf(Some(3)))

        val result = Traverse.tree.traverse(tree)(_.map(_.toString))

        result should contain(Branch(Branch("1", "2"), Leaf("3")))
      }

      "return None when at least one element in list is None" in {
        val tree = Branch(Branch(Some(1), None), Leaf(Some(3)))

        val result = Traverse.tree.traverse(tree)(_.map(_.toString))

        result shouldBe empty
      }
    }
  }

  "map" when {
    "given a function" should {
      "map values in list" in {
        val list = List(1, 2, 3)

        val result = Traverse.list.map(list)(_.toString)

        result should be(List("1", "2", "3"))
      }
    }
  }

  "foldMap" when {
    "used on a list" should {
      "map it to single value" in {
        val list = List(1, 2, 3)

        val result = Traverse.list.foldMap(list)(Monoid.string)(_.toString)

        result should be("123")
      }
    }
  }
}
