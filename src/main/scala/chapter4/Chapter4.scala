package chapter4

import scala.math.pow

object Chapter4 {

  def mean(doubles: Seq[Double]): Option[Double] =
    if (doubles.isEmpty) None
    else Some(doubles.sum / doubles.length)

  def variance(doubles: Seq[Double]): Option[Double] =
    mean(doubles).flatMap(m => mean(doubles.map(_ - m).map(pow(_, 2))))
}
