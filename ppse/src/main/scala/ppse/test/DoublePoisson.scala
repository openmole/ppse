package ppse.test


import org.apache.commons.math3.distribution.CauchyDistribution
import org.apache.commons.math3.random.RandomGeneratorFactory

import java.util.Random

object DoublePoisson {


  def createCauchy(rng: Random = new Random()) =
    new CauchyDistribution(RandomGeneratorFactory.createRandomGenerator(rng), 0.2, 0.01)


//  def sample(x: Double, y: Double, rng: Random): Double =
//    new CauchyDistribution(RandomGeneratorFactory.createRandomGenerator(rng), )

  def inverse(dist: CauchyDistribution, minX: Double, maxX: Double, minY: Double, maxY: Double): Double = {
    def x = dist.cumulativeProbability(maxX) - dist.cumulativeProbability(minX)
    def y = dist.cumulativeProbability(maxY) - dist.cumulativeProbability(minY)
    x * y
  }
}