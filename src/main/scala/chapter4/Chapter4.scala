package chapter4

import scala.math.pow

object Chapter4 {

  def mean(doubles: Seq[Double]): Option[Double] =
    if (doubles.isEmpty) None
    else Some(doubles.sum / doubles.length)

  def variance(doubles: Seq[Double]): Option[Double] = {
    val doublesMean = mean(doubles)
    doublesMean.map { mean =>
      val dividend = doubles.map(_ - mean).map(pow(_, 2)).sum
      val divisor = doubles.length
      dividend / divisor
    }
  }
}
