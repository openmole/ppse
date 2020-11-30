package ppse.em

import scala.reflect.ClassTag
import scala.util.Random




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

import cats.implicits._
import mgo.evolution.algorithm._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools._
import mgo.tools.execution._
import monocle.macros.Lenses

import scala.language.higherKinds

object EMPPSE {



  case class EMPPSEState(hitmap: HitMap)

//  case class Result(continuous: Vector[Double], discrete: Vector[Int], pattern: Vector[Int], phenotype: Vector[Double], individual: Individual)
//
//  def result(population: Vector[Individual], continuous: Vector[C], pattern: Vector[Double] => Vector[Int]) =
//    population.map { i =>
//      Result(
//        scaleContinuousValues(continuousValues.get(i.genome), continuous),
//        Individual.genome composeLens discreteValues get i,
//        pattern(i.phenotype.toVector),
//        i.phenotype.toVector,
//        i)
//    }

//  def result(pse: PPSE2, population: Vector[Individual]): Vector[Result] =
//    result(population, pse.continuous, pse.pattern)


  type Genome = Vector[Double]

  @Lenses case class Individual(
    genome: Genome,
    phenotype: Array[Double])

  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, f.toArray)
  def vectorPhenotype = Individual.phenotype composeLens arrayToVectorLens

  def initialGenomes(number: Int, continuous: Vector[C], reject: Option[Genome => Boolean], rng: scala.util.Random) = {
    def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = Vector.fill(genomeLength)(() => rng.nextDouble()).map(_())

    val rejectValue = reject.getOrElse((_: Genome) => false)

    def generate(acc: List[Genome], n: Int): Vector[Genome] =
      if (n >= number) acc.toVector
      else {
        val g = randomUnscaledContinuousValues(continuous.length, rng)
        if (rejectValue(g)) generate(acc, n)
        else generate(g :: acc, n + 1)
      }

    generate(List(), 0)
  }

//  def adaptiveBreeding(
//    lambda: Int,
//    operatorExploration: Double,
//    discrete: Vector[D],
//    pattern: Vector[Double] => Vector[Int],
//    reject: Option[Genome => Boolean]): Breeding[EMPPSEState, Individual, Genome] =
//    PPSE2Operations.adaptiveBreeding[EMPPSEState, Individual, Genome](
//      Individual.genome.get,
//      continuousValues.get,
//      continuousOperator.get,
//      discreteValues.get,
//      discreteOperator.get,
//      discrete,
//      vectorPhenotype.get _ andThen pattern,
//      buildGenome,
//      lambda,
//      reject,
//      operatorExploration,
//      EvolutionState.s[HitMap])
//
//  def elitism(pattern: Vector[Double] => Vector[Int], continuous: Vector[C]) =
//    PPSE2Operations.elitism[EMPPSEState, Individual, Vector[Double]](
//      i => values(Individual.genome.get(i), continuous),
//      vectorPhenotype.get,
//      pattern,
//      EvolutionState.s[HitMap])
//
//  def expression(phenotype: (Vector[Double], Vector[Int]) => Vector[Double], continuous: Vector[C]): Genome => Individual =
//    deterministic.expression[Genome, Vector[Double], Individual](
//      values(_, continuous),
//      buildIndividual,
//      phenotype)
//
//  def reject(pse: PPSE2) = NSGA2.reject(pse.reject, pse.continuous)
//
//  implicit def isAlgorithm: Algorithm[PPSE2, Individual, Genome, EvolutionState[HitMap]] = new Algorithm[PPSE2, Individual, Genome, EvolutionState[HitMap]] {
//    def initialState(t: PPSE2, rng: util.Random) = EvolutionState[HitMap](s = Map.empty)
//
//    override def initialPopulation(t: PPSE2, rng: scala.util.Random) =
//      deterministic.initialPopulation[Genome, Individual](
//        PPSE2.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
//        PPSE2.expression(t.phenotype, t.continuous))
//
//    def step(t: PPSE2) =
//      (s, pop, rng) =>
//        deterministic.step[EvolutionState[HitMap], Individual, Genome](
//          PPSE2.adaptiveBreeding(t.lambda, t.operatorExploration, t.discrete, t.pattern, reject(t)),
//          PPSE2.expression(t.phenotype, t.continuous),
//          PPSE2.elitism(t.pattern, t.continuous),
//          EvolutionState.generation)(s, pop, rng)
//
//  }

}

case class EMPPSE(
    lambda: Int,
    phenotype: Vector[Double] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    continuous: Vector[C] = Vector.empty,
    discrete: Vector[D] = Vector.empty,
    operatorExploration: Double = 0.1,
    reject: Option[(Vector[Double], Vector[Int]) => Boolean] = None)

object PPSE2Operations {

  def breeding[S, I, G](
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuous: Vector[C],
    pattern: I => Vector[Int],
    buildGenome: Vector[Double] => G,
    lambda: Int,
    //reject: Option[G => Boolean],
    hitmap: S => HitMap,
    components: Int,
    iterations: Int,
    tolerance: Double): Breeding[S, I, G] =
    (s, population, rng) => {

      def hitCount(cell: Vector[Int]): Int = hitmap(s).getOrElse(cell, 0)
      val hits = population.map (i => hitCount(pattern(i)) )
      val maxHits = hits.max

      def weightedPoints = (population zip hits).flatMap { case (i, h) => Vector.fill(maxHits - h)(continuousValues(genome(i))) }


      val gmm =
        EMGMM.fit(
          components = components,
          iterations = iterations,
          tolerance = tolerance,
          x = weightedPoints.map(_.toArray).toArray,
          columns = continuous.size,
          rng
        )

      def newPoints = EMGMM.toDistribution(gmm._1).sample(lambda).toVector.map(g => g.map(v => mgo.tools.clamp(v)))

      newPoints.map(p => buildGenome(p.toVector))

      // GENERATE
//
//      val ranks = population.map(i => pattern(i)) hitCountRanking(s, population, pattern, hitmap)
//      val maxRank = ranks.max
//
//
//
//
//      val breeding = applyDynamicOperators[S, I, G](
//        tournament(ranks, logOfPopulationSize),
//        genome andThen continuousValues,
//        genome andThen discreteValues,
//        continuousOperatorStatistics,
//        discreteOperatorStatistics,
//        discrete,
//        operatorExploration,
//        buildGenome)
//      val offspring = breed[S, I, G](breeding, lambda, reject)(s, population, rng)
//      randomTake(offspring, lambda, rng)
    }

//  def elitism[S, I, P: CanBeNaN](
//                                  values: I => (Vector[Double], Vector[Int]),
//                                  phenotype: I => P,
//                                  pattern: P => Vector[Int],
//                                  hitmap: monocle.Lens[S, HitMap]): Elitism[S, I] =
//    (s, population, candidates, rng) => {
//      val noNan = filterNaN(candidates, phenotype)
//      def keepFirst(i: Vector[I]) = Vector(i.head)
//      val hm2 = addHits(phenotype andThen pattern, noNan, hitmap.get(s))
//      val elite = keepNiches(phenotype andThen pattern, keepFirst)(population ++ noNan)
//      (hitmap.set(hm2)(s), elite)
//    }

}

