package ppse.test

import ppse.{Benchmark, Sampling}

import scala.util.Random

object SampleUniform extends App {
  println(
    toCSV(Sampling.uniform2D(Benchmark.sample, 10, 100, new Random(42)))
  )
}
