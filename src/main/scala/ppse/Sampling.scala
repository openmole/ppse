package ppse

import scala.util.Random

object Sampling {

  type DensityMap = Vector[Vector[Double]]

  def uniform2D(f: Vector[Double] => Vector[Double], size: Int, points: Int, random: Random) = {
    def patternFunction = Benchmark.pattern(f, Vector(size, size))

    val densities =
      (0 until points).map(_ => Vector.fill(2)(random.nextDouble())).
        map(patternFunction).
        groupBy(identity).
        view.mapValues(_.size / points.toDouble).toMap

    (0 until size).map(x =>
      (0 until size). map(y => densities.getOrElse(Vector(x, y),0.0)).toVector
    ).toVector
  }


}
