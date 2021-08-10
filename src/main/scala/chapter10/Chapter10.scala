package chapter10

object Chapter10 {
  def isSorted(seq: IndexedSeq[Int]): Boolean = {
    // Holds a tuple (isSorted, minValue, maxValue)
    val sortedMonoid = new Monoid[(Boolean, Int, Int)] {
      override def zero: (Boolean, Int, Int) =
        (true, Int.MinValue, Int.MaxValue)

      override def op(a1: (Boolean, Int, Int), a2: (Boolean, Int, Int)): (Boolean, Int, Int) = {
        // Check whether left and right are sorted, if yes then it checks
        // if maxValue from left is smaller or equal to minValue from right
        (a1._1 && a2._1 && a1._3 <= a2._2, a1._2.min(a2._2), a1._3.max(a2._3))
      }
    }
    Monoid.foldMap(seq, sortedMonoid)(i => (true, i, i))._1
  }
}
