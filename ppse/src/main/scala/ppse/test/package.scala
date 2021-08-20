package ppse

import better.files._
import org.apache.commons.math3.distribution.{MixtureMultivariateNormalDistribution, MultivariateNormalDistribution}
import org.apache.commons.math3.random.Well44497a
import org.apache.commons.math3.util.Pair
import spire.random.rng.Well44497b

import scala.jdk.CollectionConverters._

package object test {
  def toCSV(v: Vector[Vector[Double]]) =
    v.map(_.mkString(", ")).mkString("\n")

  def mixture(seed: Long = 42) = {
    val rng = new Well44497a(seed)

    val g1 = new MultivariateNormalDistribution(rng, Array(0.1, 0.1), Array(Array(0.1, 0.0), Array(0.0, 0.1)))
    val g2 = new MultivariateNormalDistribution(rng, Array(0.8, 0.8), Array(Array(0.2, 0.0), Array(0.0, 0.2)))

    new MixtureMultivariateNormalDistribution(
      rng,
      List(
        new Pair(java.lang.Double.valueOf(0.10), g1),
        new Pair(java.lang.Double.valueOf(0.90), g2)).asJava
    )
  }

  def mixture2(seed: Long = 43) = {
    val rng = new Well44497a(seed)

    val g1 = new MultivariateNormalDistribution(rng, Array(0.3, 0.2), Array(Array(0.1, 0.0), Array(0.0, 0.1)))
    val g2 = new MultivariateNormalDistribution(rng, Array(0.2, 0.3), Array(Array(0.2, 0.0), Array(0.0, 0.2)))

    new MixtureMultivariateNormalDistribution(
      rng,
      List(
        new Pair(java.lang.Double.valueOf(0.50), g1),
        new Pair(java.lang.Double.valueOf(0.50), g2)).asJava
    )
  }



}
