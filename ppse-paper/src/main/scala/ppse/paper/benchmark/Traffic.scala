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

  def modelInputs(p: Vector[Double]) =
    def minNumberOfCars = 35
    def maxNumberOfCars = 45

    def minPatience = 45.0
    def maxPatience = 55.0

    def minAcceleration = 0.004
    def maxAcceleration = 0.006

    def minDeceleration = 0.01
    def maxDeceleration = 0.03

    Vector(
      tool.scale(p(0), minNumberOfCars, maxNumberOfCars),
      tool.scale(p(1), minAcceleration, maxAcceleration),
      tool.scale(p(2), minDeceleration, maxDeceleration),
      tool.scale(p(3), minPatience, maxPatience)
    )

  def behaviour(inputs: Vector[Double], seed: Int): Vector[Double] =
    import scala.sys.process.*

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


@main def trafficRun =
  val b = Traffic.behaviour(Vector(40.0, 0.005, 0.02, 50.0), 42)
  println(b)


@main def trafficBenchmarkPPSE(result: String, generation: Int, replication: Int) =
  val resultDir = File(result)
  val resultFile = resultDir / "patterns.csv"
  val evalFile = resultDir / "eval.csv"
  resultFile.parent.createDirectories()
  resultFile.delete(true)

  val genomeSize = 4
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
        pattern =
          v =>
            val inputs = Traffic.modelInputs(v)
            val seed = random.nextInt
            val p = Traffic.pattern(Traffic.behaviour(inputs, random.nextInt))
            traceEval(inputs, p, seed)
            p
        ,
        random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r + 1111)),
        trace = Some(trace))


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
      phenotype = (r, d, _) => Traffic.behaviour(Traffic.modelInputs(d), r.nextInt),
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
        Traffic.pattern(Traffic.behaviour(Traffic.modelInputs(x), seed))


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


@main def resultTrafficPPSE(result: String, random: String, trafficError: String) =
  import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest

  val trafficErrorFile = File(trafficError)
  trafficErrorFile.delete(true)
  trafficErrorFile.parent.createDirectoryIfNotExists(true)

  def readFile(f: File) =
    f.lines.map: l =>
      val s = l.split(',')
      (s.take(2).toSeq.map(_.toInt), s.last.toDouble)
    .toMap


  val replications = File(result).list.filter(_.name.matches("[0-9]+")).toSeq.sortBy(_.name.toInt)

  val files = File(result).list.filter(_.name.matches("[0-9]+.csv")).toSeq.sortBy(_.name.dropRight(".csv".size).toInt)

  val randomPattern = ResultTrafficRandom.randomError(File(random))

  val keys = randomPattern.keys.toSeq.filter(_ != Seq(-1, -1))

  def error(m: Map[Seq[Int], Double]) =
    val patterns = keys.map(k => m.getOrElse(k, 0.0))
    val avg = keys.map(k => randomPattern.getOrElse(k, 0.0))
    jeffreysDivergence(avg, patterns)

  //println(keys)

  val errors =
    for
      r <- replications
      i <- r.list.filter(_.name.matches("[0-9]+.csv")).toSeq.sortBy(_.name.dropRight(".csv".size).toInt)
    do
      val map = readFile(i)
      val err = error(map)
      trafficErrorFile.appendLine(s"${r.name},${i.name.dropRight(".csv".size).toInt},$err")


      //test.kolmogorovSmirnovTest(avg.toArray, patterns.toArray)
//      val d =
//        keys.count: k =>
//          val p = m.getOrElse(k, 0.0)
//          val a = avgPattern.getOrElse(k, 0.0)
//          math.abs(p - a) / a < 0.3
//      d.toDouble / keys.size


object ResultTrafficRandom:
  def computeMap(p: Seq[Seq[Int]]) =
    val hits = p.size
    p.groupBy(identity).view.mapValues(_.size).map: (p, s) =>
      p -> (s.toDouble / hits)
    .toMap

  def hits(f: File) =
    f.lines.map: l =>
      l.split(",").toSeq.map(_.toInt)
    .toArray.filter(_ != Seq(-1, -1))

  def randomError(f: File) =
    computeMap(hits(f).toSeq)


@main def resultTrafficRandom(result: String, output: String) =
  import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest
  val random = scala.util.Random(42)

  val outputFile = File(output)
  outputFile.delete(true)
  outputFile.parent.createDirectories()

  val resultFile = File(result)

  val randomPattern = ResultTrafficRandom.randomError(resultFile)
  val keys = randomPattern.keys.toSeq

  def error(m: Map[Seq[Int], Double]) =
    val patterns = keys.map(k => m.getOrElse(k, 0.0))
    val avg = keys.map(k => randomPattern.getOrElse(k, 0.0))
    jeffreysDivergence(avg, patterns)

  val hits = ResultTrafficRandom.hits(resultFile)

  def sample(n: Int) =
    (0 until n).map(_ => hits(random.nextInt(hits.length)))



  for
    s <- 1000 to 100000 by 1000
    r <- 0 until 32
  do
    val e = error(ResultTrafficRandom.computeMap(sample(s)))
    outputFile.appendLine(s"$r,$s,$e")


  //(outputDirectory / "random.csv").



  //println(keys)



//test.kolmogorovSmirnovTest(avg.toArray, patterns.toArray)
//      val d =
//        keys.count: k =>
//          val p = m.getOrElse(k, 0.0)
//          val a = avgPattern.getOrElse(k, 0.0)
//          math.abs(p - a) / a < 0.3
//      d.toDouble / keys.size
