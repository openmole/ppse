package ppse.paper

import org.apache.commons.math3.stat.correlation.Covariance

/*
 * Copyright (C) 2024 Romain Reuillon
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

object tool:
  def toApacheRandom(random: util.Random) = new org.apache.commons.math3.random.RandomGenerator:
    override def setSeed(seed: Int): Unit = ???
    override def setSeed(seed: Array[Int]): Unit = ???
    override def setSeed(seed: Long): Unit = ???
    override def nextBytes(bytes: Array[Byte]): Unit = random.nextBytes(bytes)
    override def nextInt(): Int = random.nextInt()
    override def nextInt(n: Int): Int = random.nextInt(n)
    override def nextLong(): Long = random.nextLong()
    override def nextBoolean(): Boolean = random.nextBoolean()
    override def nextFloat(): Float = random.nextFloat()
    override def nextDouble(): Double = random.nextDouble()
    override def nextGaussian(): Double = random.nextGaussian()


  def toJavaRandom(generator: org.apache.commons.math3.random.RandomGenerator) = new java.util.Random:
    val initialized = true

    override def nextBoolean = generator.nextBoolean
    override def nextBytes(bytes: Array[Byte]) = generator.nextBytes(bytes)
    override def nextDouble = generator.nextDouble
    override def nextFloat = generator.nextFloat
    override def nextGaussian = generator.nextGaussian
    override def nextInt = generator.nextInt
    override def nextInt(n: Int) = generator.nextInt(n)
    override def nextLong = generator.nextLong

    override def setSeed(seed: Long) =
      // Skip the call from Random.init
      if initialized then generator.setSeed(seed)

  def mean(x: Array[Array[Double]]): Array[Double] = x.transpose.map(a=>a.sum/a.length)

  def covariance(x: Array[Array[Double]]) = new Covariance(x).getCovarianceMatrix.getData

  def percentile(data: Seq[Double], percentile: Double): Double =
    val sortedData = data.sorted
    val index = math.ceil((percentile / 100) * sortedData.length).toInt - 1
    sortedData(index)

  def linearRegression(data: Seq[Double]) =
    import org.apache.commons.math3.stat.regression.SimpleRegression
    val s = new SimpleRegression()
    s.addData(data.zipWithIndex.map((d, i) => Array(i, d)).toArray)
    (slope = s.getSlope, intercept = s.getIntercept)

  def countOscillations(data: Seq[Double], a: Double, b: Double): Int =
    val n = data.length
    val xs = (0 until n).map(_.toDouble)
    val residuals = xs.zip(data).map((x, y) => y - (a * x + b))
    val signChanges = residuals.sliding(2).count:
      case Seq(r1, r2) => r1 * r2 < 0
      case _ => false

    signChanges / 2

  def scale(v: Double, toMin: Double, toMax: Double, fromMin: Double = 0.0, fromMax: Double = 1.0): Double =
    val factor = (toMax - toMin) / (fromMax - fromMin)
    factor * (v - fromMin) + toMin