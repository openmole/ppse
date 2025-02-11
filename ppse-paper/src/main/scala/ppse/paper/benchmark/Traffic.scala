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
import scala.util.Random

object Traffic:
  def behaviour(p: Vector[Double], seed: Int): Vector[Double] =
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
  val maxRareSample = 10
  val minClusterSize = 10
  val regularisationEpsilon = 1e-6
  val dilation = 4.0

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
        dilation = dilation,
        pattern = v => Traffic.pattern(Traffic.behaviour(v, random.nextInt)),
        random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r + 1111)),
        trace = Some(trace))

    (resultDir / s"$r.csv").write:
      pdf.map: (p, l) =>
        (p ++ Seq(l)).mkString(",")
      .mkString("\n")

  Async.blocking:
    (0 until replication).map: r =>
      run(r)
    .awaitAll

@main def trafficBenchmarkPSE(result: String, generation: Int, replication: Int) =

  val resultDir = File(result)
  val resultFile = resultDir / "patterns.csv"
  resultFile.parent.createDirectories()
  resultFile.delete(true)

  val genomeSize = 3
  val lambda = 100
  val generations = generation
  val regularisationEpsilon = 1e-6
  val maxRareSample = 10

  def run(r: Int)(using Async.Spawn) = Future:
    import mgo.evolution.algorithm.*
    import mgo.evolution.*

    val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r))

    def trace(s: ppse.StepInfo) =
      resultFile.appendLine(s"$r,${s.generation * lambda},${s.likelihoodRatioMap.size}")

    val pse = NoisyPSE(
      lambda = lambda,
      phenotype = (r, d, _) => Traffic.behaviour(d, r.nextInt),
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
    (0 until replication).foreach(run)


@main def trafficRandom(result: String, nbPoints: Int) =
  val resultFile = File(result)
  resultFile.delete(true)

  val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(42))
  val resultMap = collection.mutable.HashMap[Vector[Int], Int]()

  def run(using Async.Spawn) =
    for
      points <- 1 to nbPoints
    yield
      val x = Vector.fill(3)(random.nextDouble)
      val seed = random.nextInt

      Future:
        Traffic.pattern(Traffic.behaviour(x, seed))


  val patterns = Async.blocking:
    run.awaitAll


  val size = patterns.size
  val probabilities = patterns.view.groupBy(identity).view.mapValues(_.size.toDouble / size)

  for
    (p, prob) <- probabilities
  do
    resultFile.appendLine(s"${p.mkString(",")},$prob")


@main def resultTrafficPPSE(result: String) =
  import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest
  def readFile(f: File) =
    f.lines.map: l =>
      val s = l.split(',').map(_.toDouble)
      (s.take(2).toSeq, s.last)
    .toMap

  val files = File(result).list.filter(_.name.matches("[0-9]+.csv")).toSeq.sortBy(_.name.dropRight(".csv".size).toInt)
  val maps = files.map(readFile).toArray
  val keys = maps.flatMap(_.keys).distinct

  val avgPattern =
    keys.map: k =>
      k -> maps.map(_.getOrElse(k, 0.0)).sum / maps.length
    .toMap

  val errors =
    maps.map: m =>
      val test = new KolmogorovSmirnovTest()
      val patterns = keys.map(k => m.getOrElse(k, 0.0))
      val avg = keys.map(k => avgPattern.getOrElse(k, 0.0))
      test.kolmogorovSmirnovTest(avg.toArray, patterns.toArray)
//      val d =
//        keys.count: k =>
//          val p = m.getOrElse(k, 0.0)
//          val a = avgPattern.getOrElse(k, 0.0)
//          math.abs(p - a) / a < 0.3
//      d.toDouble / keys.size

  println(errors.zip(files).mkString(","))