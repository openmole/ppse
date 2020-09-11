package ppse

import scala.util.Random

object Test extends App {

  println(Sampling.uniform2D(Benchmark.sample, 10, 100, new Random(42)))
}
