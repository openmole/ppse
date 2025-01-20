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
import gears.async.*
import gears.async.default.given

import scala.util.Random

object benchmark:
  def kullbackLeiblerDivergence(p: Seq[Double], q: Seq[Double]): Double =
    p.zip(q).map((x, y) => if y == 0 || x == 0 then 0 else x * math.log(x / y)).sum

  def jeffreysDivergence(p: Seq[Double], q: Seq[Double]): Double =
    kullbackLeiblerDivergence(p, q) + kullbackLeiblerDivergence(q, p)

  def kolmogorovSmirnovTest(p: Seq[Double], q: Seq[Double]): Double =
    p.zip(q).map((x, y) => math.abs(x-y)).max

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

    def patternDensity(ps: PatternSquare, p: Vector[Int], excludeFallBack: Boolean = false) =
      if !excludeFallBack
      then
        if isFallbackPattern(p)
        then patternDensityForRemaining(ps)
        else patternDensityForSquare(ps.squares(p.head))
      else
        val totalVolume = ps.squares.map(volume).sum
        patternDensityForSquare(ps.squares(p.head)) / totalVolume

    def volume(square: Square) = math.pow(square.size, square.dimension)

    def patternDensityForSquare(square: Square) = volume(square) / math.pow(square.grid, square.dimension)

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

  @main def patternSquareBenchmark(result: String, replications: Int, generations: Int) =
    val resultFile = File(result)

    val genomeSize = 2
    val lambda = 100
    val maxRareSample = 10
    val minClusterSize = 3
    val regularisationEpsilon = 1e-6

    val allPatterns = PatternSquare.allPatterns2D(PatternSquare.benchmarkPattern)

    resultFile.delete(true)

    def run(r: Int)(using Async.Spawn) = Future:
      println(s"Running replication $r")

      def trace(s: ppse.StepInfo) =
        if s.generation % 10 == 0 && s.generation > 0
        then
          val all = allPatterns.toSet.filterNot(PatternSquare.isFallbackPattern)
          val indexPattern = all.map(k => k -> s.likelihoodRatioMap.getOrElse(k, 0.0)).toMap
          val missed = all.size - s.likelihoodRatioMap.count((k, _) => all.contains(k))

          val error =
            val sum = indexPattern.values.sum
            val normalized =
              if sum != 0
              then indexPattern.view.mapValues(_ / sum)
              else all.map(p => (p, 0.0))

            val (p, q) =
              normalized.toSeq.map: (p, d) =>
                (PatternSquare.patternDensity(PatternSquare.benchmarkPattern, p, excludeFallBack = true), d)
              .unzip
            kolmogorovSmirnovTest(p, q)

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
        points <- 0 to nbPoints
      do
        val (x, y) = (random.nextDouble, random.nextDouble)
        val p = PatternSquare.pattern(PatternSquare.benchmarkPattern, Vector(x, y))

        resultMap.updateWith(p): hits =>
          Some(hits.getOrElse(0) + 1)

        if points % 1000 == 0
        then
          val all = allPatterns.toSet.filterNot(PatternSquare.isFallbackPattern)
          val indexPattern = all.map(k => k -> resultMap.getOrElse(k, 0).toDouble / points).toMap
          val missed = all.size - resultMap.count((k, _) => all.contains(k))

          val error =
            val (p, q) =
              indexPattern.toSeq.map: (p, d) =>
                (PatternSquare.patternDensity(PatternSquare.benchmarkPattern, p, excludeFallBack = true), d)
              .unzip

            kolmogorovSmirnovTest(p, q)

          resultFile.append(s"$r,$points,$error,$missed\n")

    (0 until replications).foreach(run)

  @main def patternSquareBenchmarkPSE(result: String, replications: Int, generations: Int) =
    import mgo.evolution.algorithm.*
    import mgo.evolution.*
    val resultFile = File(result)

    val genomeSize = 2
    val lambda = 100

    val maxRareSample = 10
    val minClusterSize = 3
    val regularisationEpsilon = 1e-6

    val allPatterns = PatternSquare.allPatterns2D(PatternSquare.benchmarkPattern)
    resultFile.delete(true)

    def run(r: Int)(using Async.Spawn) = Future:
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

            val all = allPatterns.toSet.filterNot(PatternSquare.isFallbackPattern)
            val missed = all.size - resultSet.count(all.contains)
            resultFile.append(s"$r,$points,$missed\n")
        .eval(newRNG(r))


    Async.blocking:
      (0 until replications).map(run).awaitAll


  object Traffic:
    def behaviour(p: Vector[Double], random: Random): Vector[Double] =
      import scala.sys.process.*

      def minPatience = 1.0
      def maxPatience = 100.0

      def numberOfCars = 40

      def minAcceleration = 0.001
      def maxAcceleration = 0.01

      def minDeceleration = 0.01
      def maxDeceleration = 0.1

      val inputs =
        Vector(
          numberOfCars,
          minAcceleration + p(1) * (maxAcceleration - minAcceleration),
          minDeceleration + p(2) * (maxDeceleration - minDeceleration),
          minPatience + p(0) * (maxPatience - minPatience))

      val seed = random.nextInt()

      println(s"docker exec traffic /usr/bin/traffic ${inputs.mkString(" ")} $seed")
      val lines = Process(s"docker exec traffic /usr/bin/traffic ${inputs.mkString(" ")} $seed").lazyLines(ProcessLogger.apply(_ => ()))

      if lines.size != 2
      then Vector(-1.0, -1.0)
      else
        val speed = lines(0).toDouble
        val patience = lines(1).toDouble
        Vector(speed, patience)


    def pattern(v: Vector[Double]): Vector[Int] =
      if v == Vector(-1.0, -1.0)
      then Vector(-1, -1)
      else
        val speed = (v(0) / 2.0 * 100).toInt
        val patience = v(1).toInt
        Vector(speed, patience)


  @main def trafficBenchmarkPPSE(result: String, generation: Int, replication: Int) =
    val resultDir = File(result)
    val resultFile = resultDir / "patterns.csv"
    resultFile.parent.createDirectories()
    resultFile.delete(true)

    val genomeSize = 3
    val lambda = 100
    val generations = generation
    val maxRareSample = 100
    val minClusterSize = 10
    val regularisationEpsilon = 1e-6

    def run(r: Int)(using Async.Spawn) = Future:
      val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r))

      def trace(s: ppse.StepInfo) =
        resultFile.appendLine(s"$r,${s.generation * lambda},${s.likelihoodRatioMap.size}")

      val pdf =
        ppse.evolution(
          genomeSize = genomeSize,
          lambda = lambda,
          generations = generations,
          maxRareSample = maxRareSample,
          minClusterSize = minClusterSize,
          regularisationEpsilon = regularisationEpsilon,
          pattern = v => Traffic.pattern(Traffic.behaviour(v, random)),
          random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(42)),
          trace = trace)

      (resultDir / s"$r.csv").write:
        pdf.map: (p, l) =>
          (p ++ Seq(l)).mkString(",")
        .mkString("\n")

    Async.blocking:
      (0 until replication).map: r =>
        run(r)
      .awaitAll

  @main def trafficBenchmarkPSE(result: String, generation: Int, replication: Int) =
    import mgo.evolution.algorithm.*
    import mgo.evolution.*

    val resultDir = File(result)
    val resultFile = resultDir / "patterns.csv"
    resultFile.parent.createDirectories()
    resultFile.delete(true)

    val genomeSize = 3
    val lambda = 100
    val generations = generation
    val maxRareSample = 10
    val minClusterSize = 3
    val regularisationEpsilon = 1e-6

    def run(r: Int)(using Async.Spawn) = Future:
      val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r))

      def trace(s: ppse.StepInfo) =
        resultFile.appendLine(s"$r,${s.generation * lambda},${s.likelihoodRatioMap.size}")

      val pse = NoisyPSE(
        lambda = lambda,
        phenotype = (r, d, _) => Traffic.behaviour(d, r),
        pattern = Traffic.pattern,
        continuous = Vector.fill(3)(C(0.0, 1.0)),
        maxRareSample = maxRareSample,
        aggregation = Aggregation.median
      )

      val result = pse.until(afterGeneration(generations)).eval(newRNG(r))

      val pdf = NoisyPSE.result(pse, result._2)

      (resultDir / s"$r.csv").write:
        pdf.map: r =>
          r.pattern.mkString(",")
        .mkString("\n")

    Async.blocking:
      (0 until replication).map: r =>
        run(r)
      .awaitAll

  object Hypercubes {
    case class Hypercube(center: Vector[Double], size: Double):
      def dimension = center.size

    private def inHypercube(hypercube: Hypercube, point: Vector[Double]) =
      (point zip hypercube.center).forall { (c, sc) =>
        c >= sc - hypercube.size / 2 && c < sc + hypercube.size / 2
      }

    def pattern(ps: Hypercubes, point: Vector[Double]) =
      ps.patterns.zipWithIndex.find((s, _) => inHypercube(s, point)) match
        case None => Vector(-1)
        case Some(_, i) => Vector(i)

    def patternDensity(ps: Hypercubes, p: Vector[Int], excludeFallBack: Boolean = false) =
      if !excludeFallBack
      then
        if isFallbackPattern(p)
        then patternDensityForRemaining(ps)
        else patternDensityForHypercube(ps.patterns(p.head))
      else
        val totalVolume = ps.patterns.map(volume).sum
        patternDensityForHypercube(ps.patterns(p.head)) / totalVolume

    def volume(hypercube: Hypercube) = math.pow(hypercube.size, hypercube.dimension)

    def patternDensityForHypercube(hypercube: Hypercube) = volume(hypercube)

    def patternDensityForRemaining(hypercubes: Hypercubes) = 1.0 - hypercubes.patterns.map(volume).sum

    def allPatterns(hypercubes: Hypercubes): Vector[Vector[Int]] =
      Vector(Vector(-1)) ++ {
        for
          (_, i) <- hypercubes.patterns.zipWithIndex
        yield Vector(i)
      }

    def isFallbackPattern(p: Vector[Int]) = p.head == -1

    val dim =3
    val benchmarkPattern = Hypercubes(
      Seq((Vector(0.1,0.1,0.1),0.1,10),(Vector(0.6,0.7,0.3),0.05,3)).flatMap((v,p,n) =>
        for
          x <- 0 to n
          y <- 0 to n
          z <- 0 to n
        yield Hypercubes.Hypercube(Vector(v(0)-p/2+(x*p/n),v(1)-p/2+(y*p/n),v(2)-p/2+(z*p/n)), p/Math.pow(n,dim))):_*
    )
  }

  case class Hypercubes(patterns: Hypercubes.Hypercube*)

  @main def hypercubesBenchmark(result: String, replications: Int, generations: Int) =
    val resultFile = File(result)

    val genomeSize = 3
    val lambda = 100
    val maxRareSample = 10
    val minClusterSize = 3
    val regularisationEpsilon = 1e-6

    val dim = 3
    val allPatterns = Hypercubes.allPatterns(Hypercubes.benchmarkPattern)
    resultFile.delete(true)

    def run(r: Int)(using Async.Spawn) = Future:
      println(s"Running replication $r")

      def trace(s: ppse.StepInfo) =
        if s.generation % 10 == 0 && s.generation > 0
        then
          val all = allPatterns.toSet.filterNot(Hypercubes.isFallbackPattern)
          val indexPattern = all.map(k => k -> s.likelihoodRatioMap.getOrElse(k, 0.0)).toMap
          val missed = all.size - s.likelihoodRatioMap.count((k, _) => all.contains(k))

          val error =
            val sum = indexPattern.values.sum
            val normalized =
              if sum != 0
              then indexPattern.view.mapValues(_ / sum)
              else all.map(p => (p, 0.0))

            val (p, q) =
              normalized.toSeq.map: (p, d) =>
                (Hypercubes.patternDensity(Hypercubes.benchmarkPattern, p, excludeFallBack = true), d)
              .unzip
            kolmogorovSmirnovTest(p, q)

          resultFile.append(s"$r,${s.generation * lambda},$error,$missed\n")


      ppse.evolution(
        genomeSize = genomeSize,
        lambda = lambda,
        generations = generations,
        maxRareSample = maxRareSample,
        minClusterSize = minClusterSize,
        regularisationEpsilon = regularisationEpsilon,
        pattern = Hypercubes.pattern(Hypercubes.benchmarkPattern, _),
        random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r)),
        trace = trace)

    Async.blocking:
      (0 until replications).map(run).awaitAll

  @main def hypercubesBenchmarkRandom(result: String, replications: Int, nbPoints: Int) =
    val resultFile = File(result)
    val allPatterns = Hypercubes.allPatterns(Hypercubes.benchmarkPattern)
    resultFile.delete(true)

    def run(r: Int) = Async.blocking:
      println(s"Running replication $r")

      val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r))
      val resultMap = collection.mutable.HashMap[Vector[Int], Int]()

      for
        points <- 0 to nbPoints
      do
        val (x, y, z) = (random.nextDouble, random.nextDouble, random.nextDouble)
        val p = Hypercubes.pattern(Hypercubes.benchmarkPattern, Vector(x, y, z))

        resultMap.updateWith(p): hits =>
          Some(hits.getOrElse(0) + 1)

        if points % 1000 == 0
        then
          val all = allPatterns.toSet.filterNot(Hypercubes.isFallbackPattern)
          val indexPattern = all.map(k => k -> resultMap.getOrElse(k, 0).toDouble / points).toMap
          val missed = all.size - resultMap.count((k, _) => all.contains(k))

          val error =
            val (p, q) =
              indexPattern.toSeq.map: (p, d) =>
                (Hypercubes.patternDensity(Hypercubes.benchmarkPattern, p, excludeFallBack = true), d)
              .unzip

            kolmogorovSmirnovTest(p, q)

          resultFile.append(s"$r,$points,$error,$missed\n")

    (0 until replications).foreach(run)
