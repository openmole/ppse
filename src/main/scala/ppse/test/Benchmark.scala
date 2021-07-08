package ppse.test

import breeze.linalg.DenseVector

object Benchmark {

  def pow2(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 2.0))

  def scalarP(p: Vector[Double]): Vector[Double] = {
    val pv = DenseVector(p.toArray)

    val x = DenseVector(0, 1.0)
    val y = DenseVector(1.0, 0.0)

    pv

    pv * x

    p.map(math.pow(_, 2.0))
  }


  def pattern(x: Vector[Double], g: Vector[Int]): Vector[Int] =
    x zip g map {
      case (f, g) => math.floor(f * g).toInt
    }

}
