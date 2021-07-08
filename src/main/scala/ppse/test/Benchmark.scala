package ppse.test

import breeze.linalg.DenseVector

object Benchmark {

  def pow2(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 2.0))

  def bisect(wB: Double, wO: Double)(x: Double, y: Double) = {
    def distanceToBisect(x: Double, y: Double) = math.abs(y - x) / math.sqrt(2.0)
    def distanceToOrigin(x: Double, y: Double) = math.hypot(x, y)
    (wB * distanceToBisect(x, y)) * (wO / distanceToOrigin(x, y))
  }

  def pattern(x: Vector[Double], g: Vector[Int]): Vector[Int] =
    x zip g map {
      case (f, g) => math.floor(f * g).toInt
    }

  def compareToUniformBenchmark(pattern: Vector[Double] => Vector[Int], density: Vector[(Vector[Int], Double)]) = {
    val aLot = 1000000

    val (uniformDensity, _) = SampleUniform.uniform2D(pattern, aLot)

    val deltas =
      for {
        (pp, dp) <- density
        du = uniformDensity.getOrElse(pp, 0.0)
      } yield math.abs(dp - du)

    deltas.sum
  }


}
