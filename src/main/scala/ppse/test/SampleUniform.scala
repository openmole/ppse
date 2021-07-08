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

  def uniform2D(points: Int, random: Random = new Random(42)) = {
    val size = 50

    def pattern = boundedGrid(
      lowBound = Vector(0.0, 0.0),
      highBound = Vector(1.0, 1.0),
      definition = Vector(size, size))(_)

    val drawn = (0 until points).map(_ => Vector.fill(2)(random.nextDouble())).map(Benchmark.pow2)

    val patterns =
      drawn.
        groupBy(p => pattern(p)).
        view.
        mapValues(_.size / points.toDouble).
        toMap

    drawn.map(d => d -> patterns(pattern(d)))
  }



}


object SampleUniformApp extends App {

  val max = 10000

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



  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      config.map.foreach { f =>
        def write(file: File, densities: Seq[(Vector[Double], Double)]) =
          file.write(densities.map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))

        val pattern = SampleUniform.uniform2D(max)
        f.delete(swallowIOExceptions = true)
        write(f, pattern)
      }

      config.trace.foreach { f =>
        f.delete(swallowIOExceptions = true)

        for {
          points <- 100 to 10000 by 100
        } {
          val pattern = SampleUniform.uniform2D(points)
          f.appendLine(s"$points, ${pattern.size}")
        }
      }
    case None =>
  }


}
