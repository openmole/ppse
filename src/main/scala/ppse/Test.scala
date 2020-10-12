package ppse

import scala.util.Random

object Test extends App {
  def toCSV(v: Vector[Vector[Double]]) =
    v.map(_.mkString(", ")).mkString("\n")

  println(
    toCSV(Sampling.uniform2D(Benchmark.sample, 10, 100, new Random(42)))
  )
}
