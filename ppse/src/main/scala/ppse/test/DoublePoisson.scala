package ppse.test


import org.apache.commons.math3.distribution.CauchyDistribution
import org.apache.commons.math3.random.RandomGeneratorFactory

import java.util.Random

object DoublePoisson {

  def createCauchy(rng: Random = new Random()) =
    new CauchyDistribution(RandomGeneratorFactory.createRandomGenerator(rng), 0.4, 0.01)

  def density(x: Vector[Double]): Vector[Double] =
    val dist = createCauchy()
    x.map(dist.inverseCumulativeProbability)

}