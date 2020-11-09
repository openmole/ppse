package ppse.test

import better.files._
import org.apache.commons.math3.distribution._
import org.apache.commons.math3.util._
import ppse._

import scala.jdk.CollectionConverters._

object SampleGaussian extends App {

  val g1 = new MultivariateNormalDistribution(Array(0.1, 0.1), Array(Array(0.1, 0.0), Array(0.0, 0.1)))
  val g2 = new MultivariateNormalDistribution(Array(0.8, 0.8), Array(Array(0.1, 0.0), Array(0.0, 0.1)))

  val gm = new MixtureMultivariateNormalDistribution(
    List(
      new Pair(java.lang.Double.valueOf(0.5), g1),
      new Pair(java.lang.Double.valueOf(0.5), g2)).asJava
  )

  val points = 500000
  val size = 50

//  def pattern(p: Vector[Double]): Vector[Int] = {
//    p.map(x => math.floor(x * size).toInt)
//  }

  def patternFunction =
    Benchmark.pattern(Benchmark.sample, Vector(size, size))

  val drawn = (0 until points).map(_ => gm.sample().toVector).map(s => (patternFunction(s), gm.density(s.toArray)))//.filter(_._1.forall(_ <= 50))
  val total = drawn.map(_._2).sum

  val densities =
    drawn.
      groupBy(p => p._1).
      view.
      mapValues(_.map(_._2).sum / total.toDouble).
      toMap


//  val densities =
//    (0 until points).map(_ => gm.sample().toVector).
//      groupBy(pattern).
//      view.mapValues(_.size / points.toDouble).toMap

  File(args(0)).write(densities.map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))

}
