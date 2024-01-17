package ppse.test

import scala.util.Random
import better.files.*
import breeze.stats.DescriptiveStats
import mgo.evolution.niche.boundedGrid
import ppse.test.BenchmarkUniform.uniform2D
import ppse.tool.Stat
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

  val square = PatternSquare(
    PatternSquare.Square(Vector(0.5, 0.5), 0.01, 10),
    PatternSquare.Square(Vector(0.25, 0.25), 0.01, 10),
    PatternSquare.Square(Vector(0.25, 0.75), 0.01, 10),
    PatternSquare.Square(Vector(0.75, 0.25), 0.01, 10),
    PatternSquare.Square(Vector(0.75, 0.75), 0.01, 10)
  )

  def pattern(x: Vector[Double]) = PatternSquare.pattern(square, x)
//
//    def p(x: Vector[Double]) =
//      val p = x.map(_ * 100).map(_.floor.toInt)
//      p.map(c =>
//        if c > 100 then 101
//        else if c < 0 then -1
//        else c
//      )
//    p(ExternalSquare.dilate(square, x))

//
//  def pattern(x: Vector[Double]) =
//    boundedGrid(
//      lowBound = Vector(0.0, 0.0),
//      highBound = Vector(1.0, 1.0),
//      definition = Vector(50, 50))(ExternalSquare.dilate(square, x))

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

        val allPatterns = PatternSquare.allPatterns2D(square)

//        def patternToSpace(pattern: Vector[Int]) =
//          def toSpace(x: Int) = x * 1.0 / 50.0
//          pattern.map(p => (toSpace(p), toSpace(p + 1)))

        def referenceDensity(p: Vector[Int]) = PatternSquare.patternDensity(square, p)
//          val Vector((minX, maxX), (minY, maxY)) = patternToSpace(p)
//          DoublePoisson.inverse(minX, maxX, minY, maxY)

        for
          points <- 10000 to 500000 by 10000
        do
          scribe.info(s"Computing for $points")
          val p = BenchmarkUniform.uniform2D(pattern, points)

/*
          val indexPattern =
            val all = allPatterns.toSet
            val map = p.groupMap(_._1)(_._2).view.mapValues(_.head).toMap
            map.filter((k, _) => all.contains(k))
*/
          val (indexPattern, missed) =
            val all = allPatterns.toSet
            val map = p.groupMap(_._1)(_._2).view.mapValues(_.head).toMap
            (all.map(k => (k, if map.contains(k) then map(k) else 0.0)).toMap, allPatterns.size - map.count((k, _) => all.contains(k)))

//          val avgError =
//            //allPatterns.map: p =>
//            allPatterns.filter(_ == Vector(-1, -1, -1)).map: p =>
//              val volume = if p.head == -1 then 1.0 - square.squares.map(PatternSquare.volume(_)).sum else PatternSquare.volume(square.squares(p.head))
//              val density = indexPattern.getOrElse(p, 0.0)
//              val reference = referenceDensity(p)
//            //DescriptiveStats.percentile(indexPattern.removed(Vector(-1, -1, -1)).map { (p, d) => math.abs(referenceDensity(p) - d) }, 0.5)
//              if density == 0 || reference==0 then 0 else /*volume **/ (reference*math.log(reference/density) + density*math.log(density/reference))
//            .sum / allPatterns.size
          val error =
                  val (p, q) = indexPattern.toSeq.map { (p, d) => (referenceDensity(p), d) }.unzip
                  Stat.jeffreysDivergence(p, q)
        //          val error =
//            allPatterns.map { p =>
//              val density = indexPattern.getOrElse(p, 1.0)
//              math.abs(referenceDensity(p) - density)
//            }.sum

          f.appendLine(s"$points, ${error}, ${missed}")
      }
    case None =>
  }


}
