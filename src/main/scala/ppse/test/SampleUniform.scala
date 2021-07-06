package ppse.test

import scala.util.Random
import better.files._
import ppse.test.SampleUniform.uniform2D
import scopt.OParser

object SampleUniform {

  type Pattern = Vector[Int]
  type DensityMap = Map[Pattern, Double]
  type PatternFunction = Vector[Double] => Pattern

  def uniform2D(pattern: PatternFunction, points: Int, random: Random) = {
    val drawn = (0 until points).map(_ => Vector.fill(2)(random.nextDouble())).map(pattern).map(_ -> 1.0)
    val total = drawn.map(_._2).sum

    drawn.
      groupBy(p => p._1).
      view.
      mapValues(_.map(_._2).sum / total.toDouble).
      toMap
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

  def computePatterns(points: Int) = {
    val size = 50

    def patternFunction(x: Vector[Double]) =
      Benchmark.pattern(Benchmark.sample(x), Vector(size, size))

    uniform2D(patternFunction, points, new Random(42))
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      config.map.foreach { f =>
        def write(file: File, densities: SampleUniform.DensityMap) =
          file.write(densities.map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))

        val pattern = computePatterns(max)
        f.delete(swallowIOExceptions = true)
        write(f, pattern)
      }

      config.trace.foreach { f =>
        f.delete(swallowIOExceptions = true)

        for {
          points <- 100 to 10000 by 100
        } {
          val pattern = computePatterns(points)
          f.appendLine(s"$points, ${pattern.size}")
        }
      }
    case None =>
  }


}
