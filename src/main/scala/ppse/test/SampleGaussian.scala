package ppse.test

import better.files._
import org.apache.commons.math3.distribution._
import org.apache.commons.math3.util._
import ppse._

import scala.jdk.CollectionConverters._
import ppse.tool._
import shapeless.Lazy

object SampleGaussian {

  def run = {


    val points = 100000
    val size = 50

    val gm = mixture(10)

    //  def pattern(p: Vector[Double]): Vector[Int] = {
    //    p.map(x => math.floor(x * size).toInt)
    //  }

    def patternFunction(p: Vector[Double]) = Benchmark.pattern(p, Vector(size, size))
    def sample() = {
      val x = gm.sample()
      (Benchmark.sample(x.toVector), Lazy(gm.density(x)))
    }

    //val drawn = (0 until points).map(_ => gm.sample().toVector).map(s => (patternFunction(s), 1 / gm.density(s.toArray))).filter(_._1.forall(_ <= 50))
    val sampler = new RejectionSampler(sample _, _.forall(_ <= 1.0))

    val (_, drawn) = sampler.sampleVector(points, sampler.warmup(1000))
    val total = drawn.map(_._2).sum

    drawn.
      groupBy(p => patternFunction(p._1)).
      view.
      mapValues(_.map(_._2).sum / total).
      toMap

  }
//  val densities =
//    (0 until points).map(_ => gm.sample().toVector).
//      groupBy(pattern).
//      view.mapValues(_.size / points.toDouble).toMap


}

object SampleGaussianApp extends App {
  File(args(0)).write(SampleGaussian.run.map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))
}

