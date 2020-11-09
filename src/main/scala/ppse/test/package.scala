package ppse

import better.files._
import org.apache.commons.math3.distribution.{MixtureMultivariateNormalDistribution, MultivariateNormalDistribution}
import org.apache.commons.math3.util.Pair
import ppse.Sampling.DensityMap
import scala.jdk.CollectionConverters._

package object test {
  def toCSV(v: Vector[Vector[Double]]) =
    v.map(_.mkString(", ")).mkString("\n")

  def write(file: File, densities: DensityMap) =
    file.write(densities.map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))

  def mixture() = {
    val g1 = new MultivariateNormalDistribution(Array(0.1, 0.1), Array(Array(0.1, 0.0), Array(0.0, 0.1)))
    val g2 = new MultivariateNormalDistribution(Array(0.8, 0.8), Array(Array(0.1, 0.0), Array(0.0, 0.1)))

    new MixtureMultivariateNormalDistribution(
      List(
        new Pair(java.lang.Double.valueOf(0.10), g1),
        new Pair(java.lang.Double.valueOf(0.90), g2)).asJava
    )
  }


}
