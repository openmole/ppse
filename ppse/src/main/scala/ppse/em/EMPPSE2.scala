package ppse.em

/*
 * Copyright (C) 09/11/2020 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import jsat.clustering.{ClustererBase, EMGaussianMixture, SeedSelectionMethods}
import ppse.tool
import ppse.tool.{Breeze, RejectionSampler}
import mgo.tools.Lazy

import scala.reflect.ClassTag
import scala.util.{Random, Try}
import cats.implicits._
import mgo.evolution.algorithm._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools._
import mgo.tools.execution._

import monocle._
import monocle.syntax.all._

object EMPPSE2:

  type DensityMap = Map[Vector[Int], Double]
  case class EMPPSEState(
    hitmap: HitMap = Map(),
    gmm: Option[(GMM, RejectionSampler.State)] = None,
    probabilityMap: DensityMap = Map())

  case class Result[P](continuous: Vector[Double], pattern: Vector[Int], density: Double, phenotype: Vector[Double], individual: Individual[P])

  def result[P](population: Vector[Individual[P]], state: EvolutionState[EMPPSEState], continuous: Vector[C], phenotype: P => Vector[Double], pattern: Vector[Double] ⇒ Vector[Int]) =
    val densityMap = state.focus(_.s) andThen Focus[EMPPSEState](_.probabilityMap) get
    val total = densityMap.map(_._2).sum

    population.flatMap: i =>
      val ph = phenotype(i.phenotype)
      val pa = pattern(ph)

      densityMap.get(pa).map: d =>
        Result(
          scaleContinuousValues(i.genome._1.toVector, continuous),
          pa,
          d / total,
          ph,
          i)

  def result(pse: EMPPSE2, population: Vector[Individual[Vector[Double]]], state: EvolutionState[EMPPSEState]): Vector[Result[Vector[Double]]] =
    result(population, state, pse.continuous, identity, pse.pattern)

  type Genome = (Array[Double], Double)

  case class Individual[P](
    genome: Genome,
    phenotype: P)

  def buildIndividual[P](g: Genome, f: P) = Individual(g, f)
  //def vectorPhenotype[P] = Focus[Individual[P]](_.phenotype) andThen arrayToVectorIso[Double]

  def initialGenomes(number: Int, continuous: Vector[C]/*, reject: Option[Genome => Boolean]*/, rng: scala.util.Random) =
    def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = Array.fill(genomeLength)(() => rng.nextDouble()).map(_())

    //val rejectValue = reject.getOrElse((_: Genome) => false)

    def generate(acc: List[Genome], n: Int): Vector[Genome] =
      if n >= number
      then acc.toVector
      else
        val g = randomUnscaledContinuousValues(continuous.length, rng)
        generate((g, 1.0) :: acc, n + 1)

    generate(List(), 0)

  def breeding[P](
    continuous: Vector[C],
    lambda: Int): Breeding[EvolutionState[EMPPSEState], Individual[P], Genome] =
    EMPPSE2Operations.breeding(continuous, identity[Genome] _, lambda, Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.gmm) get)

  def elitism[P: CanBeNaN](
    pattern: P => Vector[Int],
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    warmupSampler: Int,
    maxRareSample: Int,
    genomeSize: Int,
    minClusterSize: Int,
    regularisationEpsilon: Double) =
    EMPPSE2Operations.elitism[EvolutionState[EMPPSEState], Individual[P], P](
      values = _.genome,
      phenotype = (_: Individual[P]).phenotype,
      pattern = pattern,
      densityMap = Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.probabilityMap),
      hitmap = Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.hitmap),
      gmm = Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.gmm),
      iterations = iterations,
      tolerance = tolerance,
      dilation = dilation,
      warmupSampler = warmupSampler,
      maxRareSample =maxRareSample,
      genomeSize = genomeSize,
      minClusterSize = minClusterSize,
      regularisationEpsilon = regularisationEpsilon
    )

  def expression[P](phenotype: (Random, Vector[Double]) => P, continuous: Vector[C]) = (rng: Random, g: Genome) =>
    val sc = scaleContinuousValues(g._1.toVector, continuous)
    Individual(g, phenotype(rng, sc))

  implicit def isAlgorithm: Algorithm[EMPPSE2, Individual[Vector[Double]], Genome, EvolutionState[EMPPSEState]] = new Algorithm[EMPPSE2, Individual[Vector[Double]], Genome, EvolutionState[EMPPSEState]]:
    def initialState(t: EMPPSE2, rng: util.Random) = EvolutionState[EMPPSEState](s = EMPPSEState())

    override def initialPopulation(t: EMPPSE2, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      noisy.initialPopulation[Genome, Individual[Vector[Double]]](
        EMPPSE2.initialGenomes(t.lambda, t.continuous, rng),
        EMPPSE2.expression(t.phenotype, t.continuous),
        rng,
        parallel)

    def step(t: EMPPSE2) =
      noisy.step[EvolutionState[EMPPSEState], Individual[Vector[Double]], Genome](
        EMPPSE2.breeding(t.continuous, t.lambda),
        EMPPSE2.expression[Vector[Double]](t.phenotype, t.continuous),
        EMPPSE2.elitism(t.pattern, t.iterations, t.tolerance, t.dilation, t.warmupSampler, t.maxRareSample, t.continuous.size, t.minClusterSize, t.regularisationEpsilon),
        Focus[EvolutionState[EMPPSEState]](_.generation),
        Focus[EvolutionState[EMPPSEState]](_.evaluated)
      )


  def acceptPoint(x: Vector[Double]) =
    x.forall(_ <= 1.0) && x.forall(_ >= 0.0)

  def toSampler(gmm: GMM, rng: Random) =
    val distribution = GMM.toDistribution(gmm, rng)

    def sample() =
      val x = distribution.sample()
      (x.toVector, Lazy(distribution.density(x)))

    new tool.RejectionSampler(sample _, EMPPSE.acceptPoint)


case class EMPPSE2(
  lambda: Int,
  phenotype: (scala.util.Random, Vector[Double]) => Vector[Double],
  pattern: Vector[Double] => Vector[Int],
  continuous: Vector[C],
  maxRareSample: Int = 10,
  iterations: Int = 1000,
  tolerance: Double = 0.0001,
  warmupSampler: Int = 10000,
  dilation: Double = 1.0,
  minClusterSize: Int = 3,
  regularisationEpsilon: Double = 1e-6)

object EMPPSE2Operations:

  import EMPPSE2.DensityMap

  def breeding[S, I, G](
    continuous: Vector[C],
    buildGenome: ((Array[Double], Double)) => G,
    lambda: Int,
    //reject: Option[G => Boolean],
    gmm: S => Option[(GMM, RejectionSampler.State)]
  ): Breeding[S, I, G] = (s, population, rng) =>

      def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = Array.fill(genomeLength)(() => rng.nextDouble()).map(_())
      def randomIndividuals = (0 to lambda).map(_ => buildGenome(randomUnscaledContinuousValues(continuous.size, rng), 1.0)).toVector

      gmm(s) match
        case None => randomIndividuals
        case Some((gmmValue, rejectionSamplerState)) =>
          val sampler = EMPPSE2.toSampler(gmmValue, rng)
          val (_, sampled) = sampler.sampleVector(lambda, rejectionSamplerState)
          val breed = sampled.map(s => buildGenome(s._1.toArray, s._2))
          breed


  def elitism[S, I, P: CanBeNaN](
    values: I => (Array[Double], Double),
    phenotype: I => P,
    pattern: P => Vector[Int],
    densityMap: monocle.Lens[S, DensityMap],
    hitmap: monocle.Lens[S, HitMap],
    gmm: monocle.Lens[S, Option[(GMM, RejectionSampler.State)]],
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    warmupSampler: Int,
    minClusterSize: Int,
    maxRareSample: Int,
    genomeSize: Int,
    regularisationEpsilon: Double): Elitism[S, I] = (state, population, candidates, rng) =>

    def updateGMM(
      genomes: Array[Array[Double]],
      patterns: Array[Array[Int]],
      newHitMap: HitMap,
      random: Random) =

      val rareIndividuals =
        (genomes zip patterns).filter: p =>
          val hits = newHitMap.getOrElse(p._2.toVector, 0)
          hits <= maxRareSample
        .map(_._1)


      val res =
        if rareIndividuals.length < minClusterSize
        then None
        else
          Some:
            val x = rareIndividuals
            val (clusterMeans, clusterCovariances, clusterWeights) = Clustering.build(x, minClusterSize)

            def newGMM = EMGMM.fit(
              components = clusterMeans.length,
              iterations = iterations,
              tolerance = tolerance,
              x = x,
              means = clusterMeans,
              covariances = clusterCovariances,
              weights = clusterWeights,
              regularisationEpsilon = regularisationEpsilon)

            def gmmWithOutliers = EMGMM.integrateOutliers(x, newGMM._1, regularisationEpsilon)

            val dilatedGMM = GMM.dilate(gmmWithOutliers, dilation)
            val samplerState = EMPPSE2.toSampler(dilatedGMM, rng).warmup(warmupSampler)

            (dilatedGMM, samplerState)

      res

    def updateState(
      genomes: Array[Array[Double]],
      patterns: Array[Array[Int]],
      offspringGenomes: Array[(Array[Double], Double)],
      offspringPatterns: Array[Array[Int]],
      densityMap: DensityMap,
      hitMap: HitMap,
      random: Random): (HitMap, Option[(GMM, RejectionSampler.State)], DensityMap) =

      val newHitMap =
        def addHits(offspringPatterns: Array[Array[Int]], hitmap: HitMap): HitMap =
          def hits(map: HitMap, c: Vector[Int]) = map.updated(c, map.getOrElse(c, 0) + 1)
          offspringPatterns.foldLeft(hitmap)((m, p) => hits(m, p.toVector))

        addHits(offspringPatterns, hitMap)

      def newDensityMap =
        def offSpringDensities =
          val groupedGenomes = (offspringGenomes zip offspringPatterns).groupMap(_._2)(_._1)
          offspringPatterns.map: p =>
            p -> groupedGenomes(p).map(g => 1 / g._2).sum

        offSpringDensities.foldLeft(densityMap):
          case (map, (pattern, density)) =>
            map.updatedWith(pattern.toVector)(v => Some(v.getOrElse(0.0) + density))

      def newGMM =
        updateGMM(
          genomes = genomes,
          patterns = patterns,
          newHitMap = newHitMap,
          random = random
        )

      (newHitMap, newGMM, newDensityMap)

    def offSpringWithNoNan = filterNaN(candidates, phenotype)

    val newPopulation = keepRandomElementInNiches(phenotype andThen pattern, rng)(population ++ offSpringWithNoNan)

    def genomes(p: Vector[I]) = p.map(values).map(_._1).toArray
    def patterns(p: Vector[I]) = p.map(phenotype andThen pattern).map(_.toArray).toArray

    val (elitedHitMap, elitedGMM, elitedDensity) =
      updateState(
        genomes = genomes(newPopulation),
        patterns = patterns(newPopulation),
        offspringGenomes = offSpringWithNoNan.map(values).toArray,
        offspringPatterns = patterns(offSpringWithNoNan),
        densityMap = densityMap.get(state),
        hitMap = hitmap.get(state),
        random = rng)

    def state2 =
      (gmm.modify(gmm => elitedGMM) andThen
        densityMap.replace(elitedDensity) andThen
        hitmap.replace(elitedHitMap))(state)

    (state2, newPopulation)




