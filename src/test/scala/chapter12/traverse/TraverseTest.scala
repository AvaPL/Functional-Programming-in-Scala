package chapter12.traverse

import chapter10.monoid.Monoid
import chapter12.applicative1.Applicative
import chapter12.monad.Monad
import chapter3.{Branch, Leaf}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TraverseTest extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {
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

  "reverse" when {
    "used on a list" should {
      "reverse it" in {
        val list = List(1, 2, 3)

        val result = Traverse.list.reverse(list)

        result should be(List(3, 2, 1))
      }
    }

    "used on two lists" should {
      "have homomorphism with toList" in {
        forAll { (list1: List[Int], list2: List[Int]) =>
          import Traverse.list.{toList, reverse}

          val left = toList(reverse(list1)) ++ toList(reverse(list2))
          val right = reverse(toList(list2) ++ toList(list1))

          left should be(right)
        }
      }
    }
  }

  "foldLeft" when {
    "used on a list" should {
      "be an equivalent of foldLeft list method" in {
        forAll { list: List[Int] =>
          val left = Traverse.list.foldLeft(list, 0)(_ + _)
          val right = list.foldLeft(0)(_ + _)

          left should be(right)
        }
      }
    }

    "used on an option" should {
      "be an equivalent of fold option method" in {
        forAll { option: Option[Int] =>
          val left = Traverse.option.foldLeft(option, 0)(_ + _)
          val right = option.fold(0)(identity)

          left should be(right)
        }
      }
    }
  }

  "fuse" when {
    "used on list and functions returning option and lazy list" should {
      "return a tuple of (Option[List], LazyList[List])" in {
        val list = List(1, 2, 3)

        val result = Traverse.list.fuse(list)(Some(_): Option[Int], LazyList(_))(Monad.option, Monad.lazyList)

        result should be((Some(List(1, 2, 3)), LazyList(List(1, 2, 3))))
      }
    }
  }

  "compose" when {
    "given second traverse" should {
      "create a composition of traverses" in {
        val listTraverse = Traverse.list
        val treeTraverse = Traverse.tree
        val composedTraverse = listTraverse.compose(treeTraverse)

        val listOfTrees = List(Branch(1, 3), Leaf(5))
        val sum = composedTraverse.foldLeft(listOfTrees, 0)(_ + _)

        sum should be(9)
      }
    }
  }
}
