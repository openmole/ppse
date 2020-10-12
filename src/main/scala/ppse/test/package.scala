package ppse

import better.files._
import ppse.Sampling.DensityMap

package object test {
  def toCSV(v: Vector[Vector[Double]]) =
    v.map(_.mkString(", ")).mkString("\n")

  def write(file: File, densities: DensityMap) =
    file.write(densities.map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))

}
