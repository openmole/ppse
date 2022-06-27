
import better.files._

@main def pattern(file: String, result: String): Unit =
  val lines = File(file).lines
  val data = lines.drop(1).map(l => l.split(",").map(_.toDouble)).toSeq
  val size = data.size
  val metrics = data.map { d => Vector(d(0), d(5)) }

  def pattern(v: Vector[Double]) =
    val cv = if v(1) < -1.0 then -1.0 else v(1)
    Vector((v(0) * 1 / 10.0).toInt, math.ceil(cv * (1 / 0.2)).toInt)

  def value(v: Vector[Int]) = Vector(v(0).toDouble * 10, v(1) * 0.2)

  val density = metrics.groupBy(pattern).view.mapValues(_.size / size.toDouble)

  File(result).delete(swallowIOExceptions = true)
  for
    (p, d) <- density.toSeq.sortBy(d => -d._2)
  do
    File(result).appendLine(s"${value(p).mkString(",")},$d")
