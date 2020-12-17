package ppse.test

import ppse.Sampling

import scala.util.Random
import better.files._

object SampleUniform {

  def run = {
    val size = 50

    def patternFunction(x: Vector[Double]) =
      Benchmark.pattern(Benchmark.sample(x), Vector(size, size))

    Sampling.uniform2D(patternFunction, 50000, new Random(42))
  }
}


object SampleUniformApp extends App {
  write(
    File(args(0)),
    SampleUniform.run
  )
}
