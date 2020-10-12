package ppse

import scala.util.Random

object Sampling {

  type Pattern = Vector[Int]
  type DensityMap = Map[Pattern, Double]
  type PatternFunction = Vector[Double] => Pattern

  def uniform2D(pattern: PatternFunction, points: Int, random: Random) = {
    val drawn = (0 until points).map(_ => Vector.fill(2)(random.nextDouble())).map(pattern).map(_ -> 1.0)
    val total = drawn.map(_._2).sum

    drawn.
      groupBy(p => p._1).
      view.
      mapValues(_.map(_._2).sum / total.toDouble).
      toMap
  }


}
