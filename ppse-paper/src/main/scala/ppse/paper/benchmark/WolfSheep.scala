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

object WolfSheep:

  type Aggregation = (Seq[Double], Seq[Double]) => Vector[Double]

//  def buildAggregation(s: String): Aggregation =
//    import tool.percentile
//    val pattern = "([sw])(\\d+)([sw])(\\d+)".r
//
//    s match
//      case pattern(l1, v1, l2, v2) =>
//        (s, w) =>
//          val data1 = if l1 == "s" then s else w
//          val data2 = if l2 == "s" then s else w
//          Vector(percentile(data1, v1.toInt), percentile(data2, v2.toInt))
//      case _ => throw new IllegalArgumentException("Input does not match expected pattern.")


  def modelInputs(p: Vector[Double]) =
    def numberOfAgent = 500
    def maxMovementCost = 2.0
    def maxRegrowthRate = 2.0
    def maxEnergyGainFromGrass = 2.0
    def maxEnergyGainFromSheep = 2.0
    def maxMaxGrass = 10.0

    Vector(
      numberOfAgent * p(0),
      numberOfAgent * (1 - p(0)),
      maxMovementCost * p(1),
      maxRegrowthRate * p(2),
      maxEnergyGainFromGrass * p(3),
      maxEnergyGainFromSheep * p(4),
      maxMaxGrass * p(5))

  def behaviour(inputs: Vector[Double], seed: Int, aggregation: Aggregation): Vector[Double] =
    import scala.sys.process.*

    println(s"docker exec wolf-sheep /usr/bin/wolf-sheep ${inputs.mkString(" ")} $seed")
    val lines = Process(s"docker exec wolf-sheep /usr/bin/wolf-sheep ${inputs.mkString(" ")} $seed").lazyLines(ProcessLogger.apply(_ => ()))

    if lines.size != 2
    then Vector()
    else
      val sheeps = lines(0).split(",").map(_.toDouble).toSeq
      val wolfs = lines(1).split(",").map(_.toDouble).toSeq
      aggregation(sheeps, wolfs)

  def pattern(v: Vector[Double]): Vector[Int] =
    if v.isEmpty
    then Vector.fill(4)(-1000)
    else
      Vector(
        v(0).toInt,
        v(1).toInt,
        v(2).toInt,
        v(3).toInt
      )

@main def wolfSheepBenchmarkPPSE(result: String, generation: Int, replication: Int) =
  val resultDir = File(result)
  val resultFile = resultDir / "patterns.csv"
  val evalFile = resultDir / "eval.csv"
  resultDir.createDirectories()
  resultFile.delete(true)
  evalFile.delete(true)

  val genomeSize = 6
  val lambda = 100
  val generations = generation
  val maxRareSample = 10
  val minClusterSize = 10
  val regularisationEpsilon = 1e-6
  val dilation = 4.0

  val aggregation: WolfSheep.Aggregation =
    (_, wolves) =>
      val reg = tool.linearRegression(wolves)
      val slope =
        reg.slope match
          case s if s < -10 => -2
          case s if s < -5 => -1
          case s if s < 5 => 0
          case s if s < 10 => 1
          case s => 2

      val osc = tool.countOscillations(wolves, reg.slope, reg.intercept)
      val amplitude = wolves.max - wolves.min
      val median = tool.percentile(wolves, 50)

      Vector(slope.toDouble, osc.toDouble, amplitude / 10, median / 10)


  def run(r: Int)(using Async.Spawn) = Future:
    val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r))

    def save(pdf: ppse.SamplingWeightMap, eval: Int) =
      (resultDir / s"$r").createDirectories()
      (resultDir / s"$r/$eval.csv").write:
        pdf.map: (p, l) =>
          (p ++ Seq(l)).mkString(",")
        .mkString("\n")

    def trace(s: ppse.StepInfo) =
      resultFile.appendLine(s"$r,${s.generation * lambda},${s.likelihoodRatioMap.size}")
      save(s.likelihoodRatioMap, s.generation * lambda)

    def traceEval(i: Vector[Double], p: Vector[Int], seed: Int) =
      evalFile.appendLine:
        (Seq(seed) ++ i ++ p).mkString(",")

    val pdf =
      ppse.evolution(
        genomeSize = genomeSize,
        lambda = lambda,
        generations = generations,
        maxRareSample = maxRareSample,
        minClusterSize = minClusterSize,
        regularisationEpsilon = regularisationEpsilon,
        dilation = dilation,
        pattern = v =>
          val inputs = WolfSheep.modelInputs(v)
          val seed = random.nextInt
          val p = WolfSheep.pattern(WolfSheep.behaviour(inputs, random.nextInt, aggregation))
          traceEval(inputs, p, seed)
          p
        ,
        random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r + 1111)),
        trace = Some(trace))


  Async.blocking:
    (0 until replication).map: r =>
      run(r)
    .awaitAll

@main def resultWolfSheepPPSE(result: String, aggregated: String) =
  case class Line(osc: Double, amplitude: Double, median: Double, probability: Double)

  val d =
    File(result).lines.map: l =>
      val sl = l.split(",")
      Line(osc = sl(1).toDouble, amplitude = sl(2).toDouble, median = sl(3).toDouble, probability = sl(4).toDouble)
    .filterNot(_.osc == -1000)

  val normalized = normalise(d.map(_.probability).toSeq)
  val data = (d zip normalized).map((d, n) => d.copy(probability = n))

  val aggregationDirectory = File(aggregated)
  aggregationDirectory.delete(true)
  aggregationDirectory.createDirectories()

  (aggregationDirectory / "osc_amplitude.csv").write:
    data.groupBy(d => Seq(d.osc, d.amplitude)).view.
      mapValues(_.map(_.probability).sum).toSeq.map: (k, v) =>
        (k ++ Seq(v)).mkString(",")
    .mkString("\n")


  (aggregationDirectory / "osc_median.csv").write:
    data.groupBy(d => Seq(d.osc, d.median)).view.mapValues(_.map(_.probability).sum).toSeq.map: (k, v) =>
      (k ++ Seq(v)).mkString(",")
    .mkString("\n")


  (aggregationDirectory / "amplitude_median.csv").write:
    data.groupBy(d => Seq(d.amplitude, d.median)).view.mapValues(_.map(_.probability).sum).toSeq.map: (k, v) =>
      (k ++ Seq(v)).mkString(",")
    .mkString("\n")


//@main def trafficBenchmarkPSE(result: String, generation: Int, replication: Int) =
//
//  val resultDir = File(result)
//  val resultFile = resultDir / "patterns.csv"
//  resultFile.parent.createDirectories()
//  resultFile.delete(true)
//
//  val genomeSize = 3
//  val lambda = 100
//  val generations = generation
//  val regularisationEpsilon = 1e-6
//  val maxRareSample = 10
//
//  def run(r: Int)(using Async.Spawn) = Future:
//    import mgo.evolution.algorithm.*
//    import mgo.evolution.*
//
//    val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r))
//
//    def trace(s: ppse.StepInfo) =
//      resultFile.appendLine(s"$r,${s.generation * lambda},${s.likelihoodRatioMap.size}")
//
//    val pse = NoisyPSE(
//      lambda = lambda,
//      phenotype = (r, d, _) => Traffic.behaviour(d, r.nextInt),
//      pattern = Traffic.pattern,
//      continuous = Vector.fill(3)(C(0.0, 1.0)),
//      maxRareSample = maxRareSample,
//      aggregation = Aggregation.median
//    )
//
//    val result = pse.until(afterGeneration(generations)).eval(newRNG(r))
//
//    val pdf = NoisyPSE.result(pse, result._2)
//
//    (resultDir / s"$r.csv").write:
//      pdf.map: r =>
//        r.pattern.mkString(",")
//      .mkString("\n")
//
//  Async.blocking:
//    (0 until replication).foreach(run)


//@main def wolfSheepRandom(result: String, nbPoints: Int) =
//  val resultFile = File(result)
//  resultFile.delete(true)
//
//  val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(42))
//  val resultMap = collection.mutable.HashMap[Vector[Int], Int]()
//
//  def run(using Async.Spawn) =
//    for
//      points <- 1 to nbPoints
//    yield
//      val x = Vector.fill(6)(random.nextDouble)
//      val seed = random.nextInt
//
//      Future:
//        WolfSheep.pattern(WolfSheep.behaviour(x, seed, WolfSheep.buildAggregation("s50w50")))
//
//  val patterns = Async.blocking:
//    run.awaitAll
//
//  for
//    p <- patterns
//  do
//    resultFile.appendLine(s"${p.mkString(",")}")

  //val size = patterns.size
  //val probabilities = patterns.view.groupBy(identity).view.mapValues(_.size.toDouble / size)

//  for
//    (p, prob) <- probabilities
//  do
//    resultFile.appendLine(s"${p.mkString(",")},$prob")

