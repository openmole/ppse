package ppse.test

import mgo._
import ppse.em._
import mgo.evolution._
import niche._

object EMPPSETest extends App {

  val ppse = EMPPSE(
    lambda = 10,
    phenotype = Benchmark.sample,
    pattern =
      boundedGrid(
        lowBound = Vector(0.0, 0.0),
        highBound = Vector(1.0, 1.0),
        definition = Vector(50, 50)),
    continuous = Vector.fill(2)(C(0.0, 1.0)))

  def evolution =
    ppse.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation))

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(finalPopulation)

 // println(result(pse, finalPopulation).mkString("\n"))
}
