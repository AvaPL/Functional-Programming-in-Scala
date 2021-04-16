package chapter3

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeTest extends AnyWordSpec with Matchers {

  "size" when {
    "used on leaf" should {
      "return 1" in {
        Leaf(5).size should be(1)
      }
    }

    "used on branch with two leaves" should {
      "return 3" in {
        Branch(1, 2).size should be(3)
      }
    }

    "used on complex tree" should {
      "count nodes correctly" in {
        val leftBranch = Leaf(5)
        val rightBranch = Branch(3, 4)
        val tree = Branch(leftBranch, rightBranch)

        tree.size should be(5)
      }
    }
  }

  "max" when {
    "used on leaf" should {
      "return leaf value" in {
        Tree.max(Leaf(4)) should be(4)
      }
    }

    "used on branch with leaves" should {
      "return max value of two leaves" in {
        Tree.max(Branch(-5, 3)) should be(3)
      }
    }

    "used on complex tree" should {
      "return max value of tree" in {
        val leftBranch = Leaf(-3)
        val rightBranch = Branch(3, 2)
        val tree = Branch(leftBranch, rightBranch)

        Tree.max(tree) should be(3)
      }
    }
  }

  "depth" when {
    "used on leaf" should {
      "return 1" in {
        Leaf(5).depth should be(1)
      }
    }

    "used on branch with two leaves" should {
      "return 2" in {
        Branch(1, 5).depth should be(2)
      }
    }

    "used on tree with branch depths 3 and 4" should {
      "return 5" in {
        val leftBranch = Branch(Branch(1, 4), 3)
        val rightBranch = Branch(Branch(Branch(1, 2), 6), 2)
        val tree = Branch(leftBranch, rightBranch)

        leftBranch.depth should be(3)
        rightBranch.depth should be(4)
        tree.depth should be(5)
      }
    }
  }

  "map" when {
    "given a tree" should {
      "map ints to strings" in {
        val tree = Branch(5, Branch(3, -2))
        val expectedTree = Branch("5", Branch("3", "-2"))

        tree.map(_.toString) should be(expectedTree)
      }
    }
  }

  "fold" should {
    "return same results as size" in {
      val leftBranch = Leaf(5)
      val rightBranch = Branch(3, 4)
      val tree = Branch(leftBranch, rightBranch)

      val fold = tree.fold(_ => 1)(_ + _ + 1)

      fold should be(tree.size)
    }

    "return same result as max" in {
      val leftBranch = Leaf(-3)
      val rightBranch = Branch(3, 2)
      val tree = Branch(leftBranch, rightBranch)

      val fold = tree.fold(identity)(_.max(_))

      fold should be(Tree.max(tree))
    }

    "return same values as depth" in {
      val leftBranch = Branch(Branch(1, 4), 3)
      val rightBranch = Branch(Branch(Branch(1, 2), 6), 2)
      val tree = Branch(leftBranch, rightBranch)

      val fold = tree.fold(_ => 1)(_.max(_) + 1)

      fold should be(tree.depth)
    }

    "return same values as map" in {
      val tree = Branch(5, Branch(3, -2))
      val expectedTree = Branch("5", Branch("3", "-2"))

      val fold = tree.fold(value => Leaf(value.toString): Tree[String])(Branch(_, _))

      fold should be(expectedTree)
    }
  }
}
