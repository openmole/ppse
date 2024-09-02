package ppse.paper


/*
 * Copyright (C) 2022 Romain Reuillon
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

import better.files.*

object benchmark:
  def kullbackLeiblerDivergence(p: Seq[Double], q: Seq[Double]): Double =
    p.zip(q).map((x, y) => if y == 0 || x == 0 then 0 else x * math.log(x / y)).sum

  def jeffreysDivergence(p: Seq[Double], q: Seq[Double]): Double =
    kullbackLeiblerDivergence(p, q) + kullbackLeiblerDivergence(q, p)

  object PatternSquare:
    case class Square(center: Vector[Double], size: Double, grid: Int):
      def dimension = center.size

    def inSquare(square: Square, point: Vector[Double]) =
      (point zip square.center).forall: (c, sc) =>
        c >= sc - square.size / 2 && c < sc + square.size / 2

    def patternIntern(square: Square, point: Vector[Double]) =
      def grid(size: Int, x: Vector[Double]) = x.map(_ * size).map(_.floor.toInt)

      val z = 1 / square.size
      grid(square.grid, (point zip square.center).map((c, sc) => (c - sc + square.size / 2) * z))

    def pattern(ps: PatternSquare, point: Vector[Double]) =
      ps.squares.zipWithIndex.find((s, _) => inSquare(s, point)) match
        case None => Vector.fill(point.size + 1)(-1)
        case Some(s, i) => Vector(i) ++ patternIntern(s, point)

    def patternDensity(ps: PatternSquare, p: Vector[Int]) =
      p.head match
        case -1 => patternDensityForRemaining(ps)
        case i => patternDensityForSquare(ps.squares(i))

    def volume(square: Square) = math.pow(square.size, square.dimension)

    def patternDensityForSquare(square: Square) = volume(square) / math.pow(square.grid, square.dimension)

    def patternDensityForRemaining(patternSquare: PatternSquare) = 1.0 - patternSquare.squares.map(volume).sum

    def allPatterns2D(patternSquare: PatternSquare): Vector[Vector[Int]] =
      Vector(Vector(-1, -1, -1)) ++ {
        for
          (s, i) <- patternSquare.squares.zipWithIndex
          x <- 0 until s.grid
          y <- 0 until s.grid
        yield Vector(i, x, y)
      }

    val benchmarkPattern = PatternSquare(
      PatternSquare.Square(Vector(0.5, 0.5), 0.01, 10),
      PatternSquare.Square(Vector(0.25, 0.25), 0.01, 10),
      PatternSquare.Square(Vector(0.25, 0.75), 0.01, 10),
      PatternSquare.Square(Vector(0.75, 0.25), 0.01, 10),
      PatternSquare.Square(Vector(0.75, 0.75), 0.01, 10)
    )


  case class PatternSquare(squares: PatternSquare.Square*)

  @main def patternSquareBenchmark(result: String, replications: Int) =
    val resultFile = File(result)

    val genomeSize = 2
    val lambda = 100
    val generations = 500
    val maxRareSample = 10
    val minClusterSize = 3
    val regularisationEpsilon = 1e-6

    val allPatterns = PatternSquare.allPatterns2D(PatternSquare.benchmarkPattern)

    resultFile.delete(true)

    def run(r: Int) =

      println(s"Running replication $r")
      def trace(s: ppse.StepInfo) =
        if s.generation % 10 == 0
        then
          val all = allPatterns.toSet
          val indexPattern = all.map(k => k -> s.likelihoodRatioMap.getOrElse(k, 0.0)).toMap
          val missed = allPatterns.size - s.likelihoodRatioMap.count((k, _) => all.contains(k))

          val error =
            val sum = indexPattern.values.sum
            val normalized = indexPattern.view.mapValues(_ / sum)
            val (p, q) = normalized.toSeq.map((p, d) => (PatternSquare.patternDensity(PatternSquare.benchmarkPattern, p), d)).unzip
            jeffreysDivergence(p, q)

          resultFile.append(s"$r,${s.generation * lambda},$error,$missed\n")

      ppse.evolution(
        genomeSize = genomeSize,
        lambda = lambda,
        generations = generations,
        maxRareSample = maxRareSample,
        minClusterSize = minClusterSize,
        regularisationEpsilon = regularisationEpsilon,
        pattern = PatternSquare.pattern(PatternSquare.benchmarkPattern, _),
        random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r)),
        trace = trace)

    for r <- 0 until replications do run(r)

  @main def patternSquareBenchmarkRandom(result: String, replications: Int) =
    val resultFile = File(result)
    val allPatterns = PatternSquare.allPatterns2D(PatternSquare.benchmarkPattern)

    resultFile.delete(true)

    def run(r: Int) =
      println(s"Running replication $r")

      val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r))
      val resultMap = collection.mutable.HashMap[Vector[Int], Int]()

      for
        points <- 0 to 50000
      do
        val (x, y) = (random.nextDouble, random.nextDouble)
        val p = PatternSquare.pattern(PatternSquare.benchmarkPattern, Vector(x, y))

        resultMap.updateWith(p): hits =>
          Some(hits.getOrElse(0) + 1)

        if points % 1000 == 0
        then
          val all = allPatterns.toSet
          val indexPattern = all.map(k => k -> resultMap.getOrElse(k, 0).toDouble / points).toMap
          val missed = allPatterns.size - resultMap.count((k, _) => all.contains(k))

          val error =
            val (p, q) = indexPattern.toSeq.map((p, d) => (PatternSquare.patternDensity(PatternSquare.benchmarkPattern, p), d)).unzip
            jeffreysDivergence(p, q)

          resultFile.append(s"$r,$points,$error,$missed\n")

    for r <- 0 until replications do run(r)

@main def trafficBenchmark =
  def maxPatience = 50.0

  val genomeSize = 3
  val lambda = 10
  val generations = 2000
  val maxRareSample = 10
  val minClusterSize = 3
  val regularisationEpsilon = 1e-6

  val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(42))

  def behaviour(p: Vector[Double]) =
    import scala.sys.process.*

    def maxPatience = 50.0
    val inputs = Vector(p(0) * 82, p(1) * 0.01, p(2) * 0.1)
    val seed = random.nextInt()

    println(s"docker exec traffic /usr/bin/traffic ${inputs.mkString(" ")} $maxPatience $seed")
    val lines = Process(s"docker exec traffic /usr/bin/traffic ${inputs.mkString(" ")} $maxPatience $seed").lazyLines(ProcessLogger.apply(_ => ()))

    if lines.size != 2
    then Vector(-1, -1)
    else
      val speed = (lines(0).toDouble / 2.0 * 100).toInt
      val patience = (lines(1).toDouble / 100).toInt
      Vector(speed, patience)

  def trace(s: ppse.StepInfo) =
    println(s"${s.generation} generation: ${s.likelihoodRatioMap.size} patterns discovered")

  val pdf = ppse.evolution(
    genomeSize = genomeSize,
    lambda = lambda,
    generations = generations,
    maxRareSample = maxRareSample,
    minClusterSize = minClusterSize,
    regularisationEpsilon = regularisationEpsilon,
    pattern = behaviour,
    random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(42)),
    trace = trace)
