package ppse

object Benchmark {

  def exp(p: Vector[Double]): Vector[Double] = p.map(math.exp)


  def pattern(f: Vector[Double] => Vector[Double]): Vector[Double] => Vector[Int] = ???

}
