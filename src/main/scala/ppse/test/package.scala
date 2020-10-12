package ppse

package object test {
  def toCSV(v: Vector[Vector[Double]]) =
    v.map(_.mkString(", ")).mkString("\n")

}
