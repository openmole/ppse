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
  def behaviour(p: Vector[Double], seed: Int): Vector[Double] =
    import scala.sys.process.*
    def numberOfAgent = 500
    def maxMovementCost = 2.0
    def maxRegrowthRate = 2.0
    def maxEnergyGainFromGrass = 2.0
    def maxEnergyGainFromSheep = 2.0
    def maxMaxGrass = 10.0

    val inputs =
      Vector(
        numberOfAgent * p(0),
        numberOfAgent * (1 - p(0)),
        maxMovementCost * p(1),
        maxRegrowthRate * p(2),
        maxEnergyGainFromGrass * p(3),
        maxEnergyGainFromSheep * p(4),
        maxMaxGrass * p(5))

    println(s"docker exec wolf-sheep /usr/bin/wolf-sheep ${inputs.mkString(" ")} $seed")
    val lines = Process(s"docker exec wolf-sheep /usr/bin/wolf-sheep ${inputs.mkString(" ")} $seed").lazyLines(ProcessLogger.apply(_ => ()))

    if lines.size != 2
    then Vector(-1.0, -1.0)
    else
      val sheeps = lines(0).toDouble
      val wolfs = lines(1).toDouble
      Vector(sheeps, wolfs)


  def pattern(v: Vector[Double]): Vector[Int] =
    if v == Vector(-1.0, -1.0)
    then Vector(-1, -1)
    else
      Vector(
        (v(0) / 10).toInt,
        v(1).toInt
      )

@main def wolfSheepBenchmarkPPSE(result: String, generation: Int, replication: Int) =
  val resultDir = File(result)
  val resultFile = resultDir / "patterns.csv"
  resultFile.parent.createDirectories()
  resultFile.delete(true)

  val genomeSize = 6
  val lambda = 100
  val generations = generation
  val maxRareSample = 10
  val minClusterSize = 10
  val regularisationEpsilon = 1e-6
  val dilation = 4.0

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

    val pdf =
      ppse.evolution(
        genomeSize = genomeSize,
        lambda = lambda,
        generations = generations,
        maxRareSample = maxRareSample,
        minClusterSize = minClusterSize,
        regularisationEpsilon = regularisationEpsilon,
        dilation = dilation,
        pattern = v => WolfSheep.pattern(WolfSheep.behaviour(v, random.nextInt)),
        random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r + 1111)),
        trace = Some(trace))


  Async.blocking:
    (0 until replication).map: r =>
      run(r)
    .awaitAll

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


@main def wolfSheepRandom(result: String, nbPoints: Int) =
  val resultFile = File(result)
  resultFile.delete(true)

  val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(42))
  val resultMap = collection.mutable.HashMap[Vector[Int], Int]()

  def run(using Async.Spawn) =
    for
      points <- 1 to nbPoints
    yield
      val x = Vector.fill(6)(random.nextDouble)
      val seed = random.nextInt

      Future:
        WolfSheep.pattern(WolfSheep.behaviour(x, seed))


  val patterns = Async.blocking:
    run.awaitAll

  for
    p <- patterns
  do
    resultFile.appendLine(s"${p.mkString(",")}")

  //val size = patterns.size
  //val probabilities = patterns.view.groupBy(identity).view.mapValues(_.size.toDouble / size)

//  for
//    (p, prob) <- probabilities
//  do
//    resultFile.appendLine(s"${p.mkString(",")},$prob")

