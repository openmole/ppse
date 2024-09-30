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

  /*val inputFile = "/tmp/patternSquareBenchmark.csv"*/
  /*val inputFile = "/home/JPerret/devel/ppse/repli.csv"*/
  val inputFile1 = "/tmp/patternSquareBenchmark.csv"
  val inputFile2 = "/tmp/patternSquareBenchmarkRandom.csv"
  def getTraces(name: String, inputFile: String, color: Color, color2: Color) =
    val source = scala.io.Source.fromFile(inputFile)
    val data = source.getLines.map(_.split(",")).toSeq
    source.close
    val simulations = data.map(row=>row(1).toInt).distinct
    val error = simulations.map(s=>data.filter(row=>row(1).toInt == s).map(row=>row(3).toDouble))

    val stats = error.map(s=>
      val stats = new DescriptiveStatistics
      s.foreach(v=>if !v.isNaN then stats.addValue(v))
      stats
    )

    val error_mean = stats.map(_.getMean)
    val error_std = stats.map(_.getStandardDeviation)
    val error_low = stats.map(_.getPercentile(10))
    val error_median = stats.map(_.getPercentile(50))
    val error_high = stats.map(_.getPercentile(90))
  //  val patterns = Seq()
    println(simulations)
    println(error_mean)
    println(error_std)
    //val scatter = Scatter().withLine(Line().withColor(Color.RGB(255,0,0)).withWidth(4)).withX(simulations).withY(error_mean).withError_y(Data(error_high).withSymmetric(false).withArrayminus(arrayminus=error_low))
    //scatter.plot("/tmp/patternSquareBenchmark.html", Layout())//.withYaxis(Axis().withType(AxisType.Log)))
    val scatter = Scatter().withLine(Line().withColor(color).withWidth(4)).withX(simulations).withY(error_mean).withName(name)
    val errorScatter = Scatter().withX(simulations ++ simulations.reverse).withY(error_high ++ error_low.reverse).withFillcolor(color2).withFill(Fill.ToZeroX).withLine(Line().withColor(Color.RGBA(0,0,0,0.0))).withName("")
    Seq(scatter, errorScatter)
  val traces = getTraces("PPSE", inputFile1, Color.RGB(255,0,0), Color.RGBA(255,127,127,0.2)) ++ getTraces("Uniform", inputFile2, Color.RGB(0,255,0), Color.RGBA(127,255,127,0.2))
  val layout = Layout().withTitle("Results for the patternSquare benchmark")
  Plotly.plot("div-id.html", traces, layout)