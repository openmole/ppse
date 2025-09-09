package ppse.tool

import breeze.stats.DescriptiveStats
import org.apache.commons.math3.stat.correlation.Covariance

/*
 * Copyright (C) 2023 Romain Reuillon
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

object Stat:
  def mean(x: Array[Array[Double]]): Array[Double] = x.transpose.map(a=>a.sum/a.length)
  def covariance(x: Array[Array[Double]]): Array[Array[Double]] = new Covariance(x).getCovarianceMatrix.getData
  def averageDifference(p:Seq[Double], q:Seq[Double]): Double =
    DescriptiveStats.percentile(p.zip(q).map((x,y) => math.abs(x - y)), 0.5)
  def jensenShannonDivergence(p:Seq[Double], q:Seq[Double]): Double =
    def rel(x:Double, y: Double):Double = {
      if x.isNaN || y.isNaN then
        Double.NaN
      else if x > 0 && y > 0 then
        x * scala.math.log(x / y)
      else if x == 0 && y >= 0 then
        0
      else
        Double.PositiveInfinity
    }
    val p_norm = p.map(v => v / p.sum)
    val q_norm = q.map(v => v / q.sum)
    val m = p.zip(q).map((p,q)=>(p + q) / 2.0)
    val left = p.zip(m).map((x,y)=>rel(x, y))
    val right = q.zip(m).map((x,y)=>rel(x, y))
    val left_sum = left.sum
    val right_sum = right.sum
    val js = left_sum + right_sum
    scala.math.sqrt(js / 2.0)

  def kullbackLeiblerDivergence(p: Seq[Double], q:Seq[Double]): Double =
    p.zip(q).map((x,y) => if y == 0 || x == 0 then 0 else x* math.log(x/y)).sum

  def jeffreysDivergence(p: Seq[Double], q: Seq[Double]): Double =
    kullbackLeiblerDivergence(p,q) + kullbackLeiblerDivergence(q,p)