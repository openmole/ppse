package ppse.test

import better.files._
import org.apache.commons.math3.distribution._
import org.apache.commons.math3.util._
import ppse._

import scala.jdk.CollectionConverters._

/**
 * Monte Carlo estimation of the success rate of the predicate.
 *
 * @param test
 * @param pass
 */
case class MonteCarloState(test: Long = 0L, pass: Long = 0L) {
  def inverseProbability() = test.toDouble / pass
}

/**
 * Rejection sampler with a predicate and a state.
 *
 * @param dist
 * @param patternFunction
 * @param predicate
 */
class RejectionSampler(val dist: AbstractMultivariateRealDistribution, val function: Vector[Double] => Vector[Double], val predicate: Vector[Double] => Boolean) {
  def success(state: MonteCarloState) = MonteCarloState(state.test + 1, state.pass + 1)
  def fail(state: MonteCarloState) = MonteCarloState(state.test + 1, state.pass)

  def warmup(n: Int, state: MonteCarloState = MonteCarloState()): MonteCarloState =
    if(n > 0) {
      val s = dist.sample()
      val pattern = function(s.toVector)
      if (!predicate(pattern)) warmup(n - 1, fail(state))
      else warmup(n -1, success(state))
    } else state

  def sample(state: MonteCarloState = MonteCarloState()): (MonteCarloState, (Vector[Double], Double)) = {
    val s = dist.sample()
    val pattern = function(s.toVector)
    if (!predicate(pattern)) {
      // if the sample is rejected, resample and keep the failure in the state
      sample(fail(state))
    } else {
      val newState = success(state)
      // if the sample is accepted, return the state, the sample pattern and the adjusted density
      (newState, (pattern, 1 / (dist.density(s) * newState.inverseProbability())))
    }
  }
}

object SampleGaussian {

  def run = {


    val points = 500000
    val size = 50

    val gm = mixture()

    //  def pattern(p: Vector[Double]): Vector[Int] = {
    //    p.map(x => math.floor(x * size).toInt)
    //  }

    def patternFunction(p: Vector[Double]) = Benchmark.pattern(p, Vector(size, size))

    //val drawn = (0 until points).map(_ => gm.sample().toVector).map(s => (patternFunction(s), 1 / gm.density(s.toArray))).filter(_._1.forall(_ <= 50))
    val sampler = new RejectionSampler(gm, Benchmark.sample, _.forall(_ <= 1.0))

    def sample(n: Int, state: MonteCarloState, seq: Seq[(Vector[Int], Double)] = IndexedSeq()): Seq[(Vector[Int], Double)] = {
      if (n == 0) seq
      else {
        val (newState, (x, d)) = sampler.sample(state)
        sample(n - 1, newState, seq :+ (patternFunction(x), d))
      }
    }

    val drawn = sample(points, sampler.warmup(1000))
    val total = drawn.map(_._2).sum

    drawn.
      groupBy(p => p._1).
      view.
      mapValues(_.map(_._2).sum / total.toDouble).
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

