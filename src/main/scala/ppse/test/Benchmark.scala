package ppse.test

object Benchmark {

  def sample(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 2.0))

  def pattern(x: Vector[Double], g: Vector[Int]): Vector[Int] =
    x zip g map {
      case (f, g) => math.floor(f * g).toInt
    }

}
