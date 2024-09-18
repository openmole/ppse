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

package ppse.plot
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics


@main def plot =
  import plotly._
  import plotly.element._
  import plotly.element.Error._
  import plotly.layout._
  import plotly.Plotly._

  val inputFile = "/tmp/patternSquareBenchmark.csv"
  val source = scala.io.Source.fromFile(inputFile)
  val data = source.getLines.map(_.split(",")).toSeq
  source.close
  val simulations = data.map(row=>row(1).toInt).distinct
  val error = simulations.map(s=>data.filter(row=>row(1).toInt == s).map(row=>row(2).toDouble))

  val stats = error.map(s=>
    val stats = new DescriptiveStatistics
    s.foreach(v=>stats.addValue(v))
    stats
  )

  val error_mean = stats.map(_.getMean)
  val error_std = stats.map(_.getStandardDeviation)
  val error_5 = stats.map(_.getPercentile(5))
  val error_median = stats.map(_.getPercentile(50))
  val error_95 = stats.map(_.getPercentile(95))
//  val patterns = Seq()
  println(simulations)
  println(error_mean)
  println(error_std)
  val scatter = Scatter().withX(simulations).withY(error_mean).withError_y(Data(error_95).withSymmetric(false).withArrayminus(arrayminus=error_5))
  scatter.plot("/tmp/patternSquareBenchmark.html", Layout())