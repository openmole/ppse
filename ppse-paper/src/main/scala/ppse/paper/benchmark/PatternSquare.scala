package ppse.paper.benchmark

/*
 * Copyright (C) 2025 Romain Reuillon
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

import ppse.paper.*
import better.files.*
import gears.async.*
import gears.async.default.given
import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest

object PatternSquare:
  case class Square(center: Vector[Double], size: Double, grid: Int):
    def dimension = center.size

  def inSquare(square: Square, point: Vector[Double]) =
    (point zip square.center).forall: (c, sc) =>
      val lowBound = sc - square.size / 2
      val highBound = sc + square.size / 2
      c >= lowBound && c < highBound

  def patternIntern(square: Square, point: Vector[Double]) =
    def grid(size: Int, x: Vector[Double]) = x.map(_ * size).map(_.floor.toInt)

    val z = 1 / square.size
    grid(square.grid, (point zip square.center).map((c, sc) => (c - sc + square.size / 2) * z))

  def pattern(ps: PatternSquare, point: Vector[Double]) =
    ps.squares.zipWithIndex.find((s, _) => inSquare(s, point)) match
      case None => Vector.fill(point.size + 1)(-1)
      case Some(s, i) => Vector(i) ++ patternIntern(s, point)

  def patternDensity(ps: PatternSquare, p: Vector[Int]) =
    if isFallbackPattern(p)
    then patternDensityForRemaining(ps)
    else patternDensityForPatternInSquare(ps.squares(p.head))

  def volume(square: Square) = math.pow(square.size, square.dimension)

  def patternDensityForPatternInSquare(square: Square) = volume(square) / math.pow(square.grid, square.dimension)

  def patternDensityForRemaining(patternSquare: PatternSquare) = 1.0 - patternSquare.squares.map(volume).sum

  def isFallbackPattern(p: Vector[Int]) = p.head == -1

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


@main def patternSquareBenchmarkPPSE(result: String, replications: Int, generations: Int) =
  val resultFile = File(result)

  val genomeSize = 2
  val lambda = 100
  val maxRareSample = 10
  val minClusterSize = 10
  val regularisationEpsilon = 1e-6
  val dilation = 4.0

  val allPatterns = PatternSquare.allPatterns2D(PatternSquare.benchmarkPattern)

  resultFile.delete(true)

  def run(r: Int)(using Async.Spawn) = Future:
    println(s"Running replication $r")

    def trace(s: ppse.StepInfo) =
      if s.generation % 10 == 0 && s.generation > 0
      then
        val all = allPatterns.filterNot(PatternSquare.isFallbackPattern)
        val indexPattern = all.map(k => k -> s.likelihoodRatioMap.getOrElse(k, 0.0)).toMap
        val missed = all.size - s.likelihoodRatioMap.count((k, _) => all.contains(k))

        val error =
          val (ref, q) =
            indexPattern.toSeq.map: (p, d) =>
              (PatternSquare.patternDensity(PatternSquare.benchmarkPattern, p), d)
            .unzip

          kullbackLeiblerDivergence(ref, q)

        resultFile.append(s"$r,${s.generation * lambda},$error,$missed\n")


    ppse.evolution(
      genomeSize = genomeSize,
      lambda = lambda,
      generations = generations,
      maxRareSample = maxRareSample,
      minClusterSize = minClusterSize,
      regularisationEpsilon = regularisationEpsilon,
      dilation = dilation,
      pattern = PatternSquare.pattern(PatternSquare.benchmarkPattern, _),
      random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r)),
      trace = Some(trace))

  Async.blocking:
    (0 until replications).map(run).awaitAll


@main def patternSquareBenchmarkRandom(result: String, replications: Int, nbPoints: Int) =
  val resultFile = File(result)
  val allPatterns = PatternSquare.allPatterns2D(PatternSquare.benchmarkPattern)

  resultFile.delete(true)

  def run(r: Int) = Async.blocking:
    println(s"Running replication $r")

    val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r))
    val resultMap = collection.mutable.HashMap[Vector[Int], Int]()

    for
      points <- 1 to nbPoints
    do
      val (x, y) = (random.nextDouble, random.nextDouble)
      val p = PatternSquare.pattern(PatternSquare.benchmarkPattern, Vector(x, y))

      resultMap.updateWith(p): hits =>
        Some(hits.getOrElse(0) + 1)

      if points % 1000 == 0
      then
        val all = allPatterns.filterNot(PatternSquare.isFallbackPattern)
        val indexPattern = all.map(k => k -> resultMap.getOrElse(k, 0).toDouble / points).toMap
        val missed = all.size - resultMap.count((k, _) => all.contains(k))

        val error =
          val (ref, q) =
            indexPattern.toSeq.map: (p, d) =>
              (PatternSquare.patternDensity(PatternSquare.benchmarkPattern, p), d)
            .unzip

          kullbackLeiblerDivergence(ref, q)

        resultFile.append(s"$r,$points,$error,$missed\n")

  (0 until replications).foreach(run)

@main def patternSquareBenchmarkPSE(result: String, replications: Int, generations: Int) =
  val resultFile = File(result)

  val genomeSize = 2
  val lambda = 100

  val maxRareSample = 10
  val minClusterSize = 3
  val regularisationEpsilon = 1e-6

  val allPatterns = PatternSquare.allPatterns2D(PatternSquare.benchmarkPattern)
  resultFile.delete(true)

  def run(r: Int)(using Async.Spawn) = Future:
    import mgo.evolution.algorithm.*
    import mgo.evolution.*

    println(s"Running replication $r")

    val pse = PSE(
      lambda = lambda,
      phenotype = (d, _) => d,
      pattern = x => PatternSquare.pattern(PatternSquare.benchmarkPattern, x),
      continuous = Vector.fill(2)(C(0.0, 1.0)),
      maxRareSample = maxRareSample)

    pse
      .until(afterGeneration(generations))
      .trace: (s, is) =>
        if s.generation % 10 == 0 && s.generation > 0
        then
          val points = s.evaluated
          val resultSet =
            PSE.result(pse, is).map: i =>
              i.pattern
            .toSet

          val all = allPatterns.filterNot(PatternSquare.isFallbackPattern)
          val missed = all.size - resultSet.count(all.contains)
          resultFile.append(s"$r,$points,$missed\n")
      .eval(newRNG(r))


  Async.blocking:
    (0 until replications).map(run).awaitAll

