package ppse.test

import better.files.File
import org.apache.commons.math3.distribution.MixtureMultivariateNormalDistribution
import ppse.tool.RejectionSampler
import shapeless.Lazy

/*
 * Copyright (C) 2021 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

object SampleMultiGaussian  {

  def run() = {
    val size = 50

    val gm1 = mixture(10)
    val gm2 = mixture2(11)

    //  def pattern(p: Vector[Double]): Vector[Int] = {
    //    p.map(x => math.floor(x * size).toInt)
    //  }

    def patternFunction(p: Vector[Double]) = Benchmark.pattern(p, Vector(size, size))

    def sample(gm: MixtureMultivariateNormalDistribution) = {
      val x = gm.sample()
      (Benchmark.sample(x.toVector), Lazy(gm.density(x)))
    }

    //val drawn = (0 until points).map(_ => gm.sample().toVector).map(s => (patternFunction(s), 1 / gm.density(s.toArray))).filter(_._1.forall(_ <= 50))

    val sampler1 = new RejectionSampler(() => sample(gm2), _.forall(_ <= 1.0))
    val sampler2 = new RejectionSampler(() => sample(gm2), _.forall(_ <= 1.0))

    val (_, drawn1) = sampler1.sampleVector(7000, sampler1.warmup(1000))
    val (_, drawn2) = sampler2.sampleVector(3000, sampler2.warmup(1000))

    val drawn = drawn1 ++ drawn2

    val total = drawn.map(_._2).sum

    drawn.
      groupBy(p => patternFunction(p._1)).
      view.
      mapValues(_.map(_._2).sum / total).
      toMap
  }
}


object SampleMultiGaussianApp extends App {
  File(args(0)).write(SampleMultiGaussian.run.map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))
}
