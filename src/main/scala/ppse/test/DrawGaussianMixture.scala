package ppse.test

import org.apache.commons.math3.distribution._
import org.apache.commons.math3.util._
import scala.jdk.CollectionConverters._

object DrawGaussianMixture extends App {

  val g1 = new MultivariateNormalDistribution(Array(0.25, 0.25), Array(Array(0.1, 0), Array(0.0, 0.1)))
  val g2 = new MultivariateNormalDistribution(Array(0.6, 0.6), Array(Array(0.2, 0.0), Array(0.0, 0.2)))

  val gm = new MixtureMultivariateNormalDistribution(
    List(new Pair(java.lang.Double.valueOf(0.5), g1), new Pair(java.lang.Double.valueOf(0.5), g2)).asJava
  )

  println(toCSV(gm.sample(100).map(_.toVector).toVector))

}
