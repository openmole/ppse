package ppse.test


import org.apache.commons.math3.distribution.CauchyDistribution
import org.apache.commons.math3.random.RandomGeneratorFactory

import java.util.Random

object DoublePoisson {

  def createCauchy(rng: Random = new Random()) =
    new CauchyDistribution(RandomGeneratorFactory.createRandomGenerator(rng), 0.4, 0.01)

  def inverse(minX: Double, maxX: Double, minY: Double, maxY: Double, dist: CauchyDistribution = createCauchy()): Double = {
    def x = dist.cumulativeProbability(maxX) - dist.cumulativeProbability(minX)
    def y = dist.cumulativeProbability(maxY) - dist.cumulativeProbability(minY)
    x * y
  }
  def density(x: Vector[Double]): Vector[Double] =
    val dist = createCauchy()
    x.map(dist.inverseCumulativeProbability)

}