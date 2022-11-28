package ppse.test

import scala.util.Random
import better.files._
import mgo.evolution.niche.boundedGrid
import ppse.test.BenchmarkUniform.uniform2D
import scopt.OParser

object BenchmarkUniform {

  type Pattern = Vector[Int]
  type DensityMap = Map[Pattern, Double]

  def uniform2D(pattern: Vector[Double] => Vector[Int], points: Int, random: Random = new Random(42)) = {
    val drawn = (0 until points).map(_ => Vector.fill(2)(random.nextDouble()))

    val patterns =
      drawn.
        groupBy(p => pattern(p)).
        view.
        mapValues(_.size / points.toDouble).
        toMap

    patterns
    //(patterns, drawn.map(d => d -> patterns(pattern(d))))
  }

  def uniformOutput(f: Vector[Double] => Vector[Double], pattern: Vector[Double] => Vector[Int], points: Int, random: Random = new Random(42), dimension: Int = 2) = {
    val drawn = (0 until points).map(_ => Vector.fill(dimension)(random.nextDouble()))

    val patterns =
      drawn.
        groupBy(p => pattern(f(p))).
        view.
        mapValues(_.size / points.toDouble).
        toMap

    drawn.map(f).map(f => f -> patterns(pattern(f)))
  }
}


@main def benchmarkUniform(args: String*) = {

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


  def pattern(x: Vector[Double]) =
    boundedGrid(
      lowBound = Vector(0.0, 0.0),
      highBound = Vector(1.0, 1.0),
      definition = Vector(50, 50))(DoublePoisson.density(x))

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
//      config.map.foreach { f =>
//        def write(file: File, densities: Seq[(Vector[Double], Double)]) =
//          file.write(densities.filterNot(p => p._1.exists(_ > 1.0) || p._1.exists(_ < 0.0)).map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))
//
//        val p = BenchmarkUniform.uniformOutput(func, pattern, max, new Random(42), dimension = 2)
//
//        //println(s"Delta to uniform for $max points ${Benchmark.compareToUniformBenchmark(Benchmark.pow andThen pattern, p.toVector)}")
//
//        f.delete(swallowIOExceptions = true)
//        write(f, p)
//      }

      config.trace.foreach { f =>
        f.delete(swallowIOExceptions = true)

        val allPatterns =
          for
            x <- 0 until 50
            y <- 0 until 50
          yield Vector(x, y)

        def patternToSpace(pattern: Vector[Int]) =
          def toSpace(x: Int) = x * 1.0 / 50.0
          pattern.map(p => (toSpace(p), toSpace(p + 1)))

        def referenceDensity(p: Vector[Int]) =
          val Vector((minX, maxX), (minY, maxY)) = patternToSpace(p)
          DoublePoisson.inverse(minX, maxX, minY, maxY)

        for
          points <- 1000 to 50000 by 1000
        do
          val p = BenchmarkUniform.uniform2D(pattern, points)

          val indexPattern = p.groupMap(_._1)(_._2).view.mapValues(_.head).toMap

          val error =
            allPatterns.map { p =>
              val density = indexPattern.getOrElse(p, 0.0)
              math.abs(referenceDensity(p) - density)
            }.sum

          f.appendLine(s"$points, ${error}")
      }
    case None =>
  }


}
