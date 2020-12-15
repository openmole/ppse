package ppse.test

object Benchmark {

  def sample(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 2.0))

  def pattern(f: Vector[Double] => Vector[Double], g: Vector[Int]): Vector[Double] => Vector[Int] =
    x => f(x) zip g map {
      case (f, g) => math.floor(f * g).toInt
    }

}
