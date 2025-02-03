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


object SEIRModel:

  case class SEIRState(S: Double, E: Double, I: Double, R: Double)

  def update(state: SEIRState, beta: Double, sigma: Double, gamma: Double, dt: Double): SEIRState =
    val dS = -beta * state.S * state.I * dt
    val dE = (beta * state.S * state.I - sigma * state.E) * dt
    val dI = (sigma * state.E - gamma * state.I) * dt
    val dR = gamma * state.I * dt

    SEIRState(
      S = state.S + dS,
      E = state.E + dE,
      I = state.I + dI,
      R = state.R + dR
    )

  def simulate(
                initialState: SEIRState,
                beta: Double,
                sigma: Double,
                gamma: Double,
                dt: Double,
                steps: Int): Seq[SEIRState] =
    (1 to steps).scanLeft(initialState): (currentState, _) =>
      update(currentState, beta, sigma, gamma, dt)

  def behaviour(p: Vector[Double]): Vector[Double] =

    val population = 1000.0
    val initialInfected = 10.0
    val initialExposed = 0.0
    val initialRecovered = 0.0
    val initialSusceptible = population - initialInfected - initialExposed - initialRecovered

    val beta = 0.4 + p(0) * (0.5 - 0.4)
    val sigma = 0.3 + p(1) * (0.4 - 0.3)
    val gamma = 0.1 + p(2) * (0.2 - 0.1)

    val dt = 0.1
    val steps = 1000

    val initialState = SEIRState(
      S = initialSusceptible / population,
      E = initialExposed / population,
      I = initialInfected / population,
      R = initialRecovered / population
    )

    val results = simulate(initialState, beta, sigma, gamma, dt, steps)

    val infectedResult = results.map(_.I * population)
    val pic = infectedResult.max
    val picTime = infectedResult.indexWhere(_ >= pic)

    Vector(pic, picTime.toDouble)


  def pattern(v: Vector[Double]): Vector[Int] = v.map(x => (x / 10).toInt)

@main def SEIRBenchmarkPPSE(result: String, generation: Int, replication: Int) =
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
        dilation = 4.0,
        pattern = v => SEIRModel.pattern(SEIRModel.behaviour(v)),
        random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r + 1111)),
        trace = trace)

    (resultDir / s"$r.csv").write:
      pdf.map: (p, l) =>
        (p ++ Seq(l)).mkString(",")
      .mkString("\n")

  Async.blocking:
    (0 until replication).map: r =>
      run(r)
    .awaitAll
