package ppse.test

import scala.util.Random
import better.files._
import mgo.evolution.niche.boundedGrid
import ppse.test.SampleUniform.uniform2D
import scopt.OParser

object SampleUniform {

  type Pattern = Vector[Int]
  type DensityMap = Map[Pattern, Double]
  type PatternFunction = Vector[Double] => Pattern

  def uniform2D(pattern: Vector[Double] => Vector[Int], points: Int, random: Random = new Random(42)) = {

    val drawn = (0 until points).map(_ => Vector.fill(2)(random.nextDouble())).map(Benchmark.inverseFlower())

    val patterns =
      drawn.
        groupBy(p => pattern(p)).
        view.
        mapValues(_.size / points.toDouble).
        toMap

    (patterns, drawn.map(d => d -> patterns(pattern(d))))
  }



}


object SampleUniformApp extends App {

  val max = 50000

  case class Config(
    map: Option[File] = None,
    trace: Option[File] = None)

  val builder = OParser.builder[Config]

  val parser = {
    import builder._
    OParser.sequence(
      programName("ppsetest"),
      head("scopt", "4.x"),
      opt[String]('m', "map").action((x, c) => c.copy(map = Some(File(x)))),
      opt[String]('t', "trace").action((x, c) => c.copy(trace = Some(File(x))))
    )
  }

  val size = 50

  def pattern = boundedGrid(
    lowBound = Vector(0.0, 0.0),
    highBound = Vector(1.0, 1.0),
    definition = Vector(size, size))(_)

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      config.map.foreach { f =>
        def write(file: File, densities: Seq[(Vector[Double], Double)]) =
          file.write(densities.map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))

        val p = SampleUniform.uniform2D(pattern, max)

        println(s"Delta to uniform for $max points ${Benchmark.compareToUniformBenchmark(pattern, p._1.toVector)}")

        f.delete(swallowIOExceptions = true)
        write(f, p._2)
      }

      config.trace.foreach { f =>
        f.delete(swallowIOExceptions = true)

        for {
          points <- 100 to max by 100
        } {
          val p = SampleUniform.uniform2D(pattern, points)._1
          f.appendLine(s"$points, ${p.size}")
        }
      }
    case None =>
  }


}
