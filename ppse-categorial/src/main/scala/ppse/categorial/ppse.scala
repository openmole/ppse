package ppse.categorial

import org.apache.commons.math3.distribution.{EnumeratedDistribution, MixtureMultivariateNormalDistribution, MultivariateNormalDistribution}
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.util.Pair
import ppse.paper.tool

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Random
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

type Genome = (Vector[Double], Double)
type Pattern = Vector[Int]
type HitMap = Map[Vector[Int], Int]
type SamplingWeightMap = Map[Vector[Int], Double]

case class Individual(genome: Genome, pattern: Pattern)
case class StepInfo(population: Vector[Individual], generation: Int)

def crossover(i1: Individual, i2: Individual) = ???

def breeding(population: Vector[Individual], genomeSize: Int, size: Int, maxRareSample: Int, hitMap: HitMap, random: Random): Vector[Genome] =
  def allAtMaxSample = population.filterNot(i => hitMap.getOrElse(i.pattern, 0) >= maxRareSample)

  if allAtMaxSample.isEmpty
  then
    Vector.fill(size):
      Vector.fill(genomeSize)(random.nextDouble()) -> 1.0
  else
    val weights =
      val w = allAtMaxSample.map(p => hitMap(p.pattern)).map(p => math.log(1 + random.nextDouble) / p)
      val total = w.sum
      w.map(_ / total)

    Vector.fill(size):
      mutate(allAtMaxSample.map(_.genome), genomeSize, weights, 0.005, random)

def updateHitMap(offspringPopulation: Vector[Individual], hitMap: HitMap): HitMap =
  val newMap = collection.mutable.Map[Vector[Int], Int]() ++ hitMap
  for
    i <- offspringPopulation
  do
    newMap.updateWith(i.pattern):
      case None => Some(1)
      case Some(v) => Some(v + 1)

  newMap.toMap

def updateWeightMap(offspringGenomes: Vector[Genome], offspringPatterns: Vector[Vector[Int]], likelihoodRatioMap: SamplingWeightMap) =
  def offSpringDensities =
    val groupedGenomes = (offspringGenomes zip offspringPatterns).groupMap(_._2)(_._1)
    groupedGenomes.view.mapValues(v => v.map((_, density) => 1 / density).sum).toSeq

  def updatePatternDensity(map: SamplingWeightMap, pattern: Vector[Int], density: Double): SamplingWeightMap =
    map.updatedWith(pattern.toVector)(v => Some(v.getOrElse(0.0) + density))

  offSpringDensities.foldLeft(likelihoodRatioMap) { case (map, (pattern, density)) => updatePatternDensity(map, pattern, density) }

def elitism(population: Vector[Individual], offspringPopulation: Vector[Individual]): Vector[Individual] =
  (population ++ offspringPopulation)
    .groupBy(_.pattern)
    .toVector
    .map(_._2.head)

def mutate(genomes: Vector[Genome], genomeSize: Int, weights: Vector[Double], sigmaMax: Double, random: Random): Genome =
  val apacheRandom = tool.toApacheRandom(random)
  val comp = (genomes zip weights).map((p, w) => new Pair[Vector[Double], java.lang.Double](p._1,w)).toList.asJava
  val dist = new EnumeratedDistribution[Vector[Double]](apacheRandom, comp)
  // We generate a covariance matrix using an identity matrix multiplied by the sigma
  val matrix = MatrixUtils.createRealIdentityMatrix(genomeSize).scalarMultiply(sigmaMax).getData
  val list = genomes.zip(weights).map((g, w) => new Pair[java.lang.Double,MultivariateNormalDistribution](w, new MultivariateNormalDistribution(g._1.toArray,matrix))).toList.asJava
  val mixture = new MixtureMultivariateNormalDistribution(apacheRandom, list)
  val choice = mixture.sample()
  (choice.toVector, mixture.density(choice))

def computePDF(likelihoodRatioMap: SamplingWeightMap) =
  val totalDensity = likelihoodRatioMap.values.sum
  likelihoodRatioMap.map((p, density) => (p, density / totalDensity))

@tailrec
def evolution(
  genomeSize: Int,
  lambda: Int,
  generations: Int,
  pattern: Vector[Double] => Vector[Int],
  population: Vector[Individual] = Vector(),
  hitMap: HitMap = Map(),
  likelihoodRatioMap: SamplingWeightMap = Map(),
  generation: Int = 0,
  maxRareSample: Int = 10,
  trace: Option[StepInfo => Unit] = None,
  random: Random): SamplingWeightMap =

  trace.foreach(f => f(StepInfo(population, generation)))

  if generation >= generations
  then computePDF(likelihoodRatioMap)
  else
    val offspringGenome = breeding(population, genomeSize, lambda, maxRareSample, hitMap, random)
    val patterns = offspringGenome.map((g, _) => pattern(g))
    val offspringPopulation = (offspringGenome zip patterns).map((g, p) => Individual(g, p))
    val newPopulation = elitism(population, offspringPopulation)
    val newHitMap = updateHitMap(offspringPopulation, hitMap)
    val newLikelihoodRatioMap = updateWeightMap(offspringGenome, patterns, likelihoodRatioMap)
    evolution(
      genomeSize,
      lambda,
      generations,
      pattern,
      newPopulation,
      newHitMap,
      newLikelihoodRatioMap,
      generation + 1,
      maxRareSample,
      trace,
      random)
