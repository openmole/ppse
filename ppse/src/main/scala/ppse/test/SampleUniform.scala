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


@main def sampleUniform(args: String*) = {

  val func = DoublePoisson.density _
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

  val size = 20

  def pattern = boundedGrid(
    lowBound = Vector(0.0, 0.0),
    highBound = Vector(1.0, 1.0),
    definition = Vector(size, size))(_)

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      config.map.foreach { f =>
        def write(file: File, densities: Seq[(Vector[Double], Double)]) =
          file.write(densities.filterNot(_._1.exists(_ > 1.0)).map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))

        val p = SampleUniform.uniformOutput(func, pattern, max, new Random(42), dimension = 2)

        //println(s"Delta to uniform for $max points ${Benchmark.compareToUniformBenchmark(Benchmark.pow andThen pattern, p.toVector)}")

        f.delete(swallowIOExceptions = true)
        write(f, p)
      }

//      config.trace.foreach { f =>
//        f.delete(swallowIOExceptions = true)
//
//        val unif = Benchmark.powDensityMap(Vector(50, 50))
//
//        for {
//          points <- 100 to max by 100
//        } {
//          val p = SampleUniform.uniform2D(pattern, points)
//          def deltaToUniform = Benchmark.compareToUniformBenchmark(p.toVector, unif.toVector)
//          f.appendLine(s"$points, ${deltaToUniform}")
//        }
//      }
    case None =>
  }


}
