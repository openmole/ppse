package ppse.test


import org.apache.commons.math3.distribution.CauchyDistribution
import org.apache.commons.math3.random.RandomGeneratorFactory

import java.util.Random

object DoublePoisson {


  def createCauchy(rng: Random = new Random()) =
    new CauchyDistribution(RandomGeneratorFactory.createRandomGenerator(rng), 0.2, 0.01)


//  def sample(x: Double, y: Double, rng: Random): Double =
//    new CauchyDistribution(RandomGeneratorFactory.createRandomGenerator(rng), )


}