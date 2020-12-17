package ppse.em

import ppse.tool
import ppse.tool.RejectionSampler
import shapeless.Lazy

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

  type ProbabilityMap = Map[Vector[Int], Double]
  @Lenses case class EMPPSEState(
    hitmap: HitMap = Map(),
    gmm: Option[(GMM, RejectionSampler.State)] = None,
    probabilityMap: ProbabilityMap = Map())

 case class Result(continuous: Vector[Double], pattern: Vector[Int], density: Double, phenotype: Vector[Double], individual: Individual)

  def result(population: Vector[Individual], state: EvolutionState[EMPPSEState], continuous: Vector[C], pattern: Vector[Double] => Vector[Int]) = {
    val densityMap = (EvolutionState.s composeLens EMPPSEState.probabilityMap).get(state)

    population.map { i =>
      val p = pattern(i.phenotype.toVector)

      Result(
        scaleContinuousValues(i.genome, continuous),
        p,
        densityMap.getOrElse(p, 0.0),
        i.phenotype.toVector,
        i)
    }
  }

  def result(pse: EMPPSE, population: Vector[Individual], state: EvolutionState[EMPPSEState]): Vector[Result] =
    result(population, state, pse.continuous, pse.pattern)

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

  def breeding(
    continuous: Vector[C],
    lambda: Int): Breeding[EvolutionState[EMPPSEState], Individual, Genome] =
    PPSE2Operations.breeding(continuous, identity, lambda, (EvolutionState.s composeLens EMPPSEState.gmm).get)

  def elitism(
    continuous: Vector[C],
    pattern: Vector[Double] => Vector[Int],
    components: Int,
    iterations: Int,
    tolerance: Double,
    lowest: Int,
    warmupSampler: Int) =
    PPSE2Operations.elitism[EvolutionState[EMPPSEState], Individual, Vector[Double]](
      continuous = continuous,
      values = Individual.genome.get,
      phenotype = Individual.phenotype composeLens arrayToVectorLens get,
      pattern = pattern,
      probabilities = EvolutionState.s composeLens EMPPSEState.probabilityMap,
      hitmap = EvolutionState.s composeLens EMPPSEState.hitmap,
      gmm = EvolutionState.s composeLens EMPPSEState.gmm,
      components = components,
      iterations = iterations,
      tolerance = tolerance,
      lowest = lowest,
      warmupSampler = warmupSampler
    )


//  def elitism[S, I, P: CanBeNaN](
//    continuous: Vector[C],
//    values: I => Vector[Double],
//    phenotype: I => P,
//    pattern: P => Vector[Int],
//    hitmap: monocle.Lens[S, HitMap],
//    gmm: monocle.Lens[S, Option[GMM]],
//    components: Int,
//    iterations: Int,
//    tolerance: Double,
//    bootstrap: Int): Elitism[EMPPSEState, Individual, Genome] =
//    PPSE2Operations.elitism(
//
//    )

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
  def expression(phenotype: Vector[Double] => Vector[Double], continuous: Vector[C]): Genome => Individual = (g: Genome) => {
    val sc = scaleContinuousValues(g, continuous)
    Individual(g, phenotype(sc).toArray)
  }
//  deterministic.expression[Genome, Vector[Double], Individual](
//      values(_, continuous),
//      buildIndividual,
//      phenotype)
//}


  //
//  def reject(pse: PPSE2) = NSGA2.reject(pse.reject, pse.continuous)

  implicit def isAlgorithm: Algorithm[EMPPSE, Individual, Genome, EvolutionState[EMPPSEState]] = new Algorithm[EMPPSE, Individual, Genome, EvolutionState[EMPPSEState]] {
    def initialState(t: EMPPSE, rng: util.Random) = EvolutionState[EMPPSEState](s = EMPPSEState())

    override def initialPopulation(t: EMPPSE, rng: scala.util.Random) =
      deterministic.initialPopulation[Genome, Individual](
        EMPPSE.initialGenomes(t.lambda, t.continuous, None, rng),
        EMPPSE.expression(t.phenotype, t.continuous))

    def step(t: EMPPSE) =
      (s, pop, rng) =>
        deterministic.step[EvolutionState[EMPPSEState], Individual, Genome](
          EMPPSE.breeding(t.continuous, t.lambda),
          EMPPSE.expression(t.phenotype, t.continuous),
          EMPPSE.elitism(t.continuous, t.pattern, t.components, t.iterations, t.tolerance, t.lowest, t.warmupSampler),
          EvolutionState.generation)(s, pop, rng)

  }

  def acceptPoint(x: Vector[Double]) =
    x.forall(_ <= 1.0) && x.forall(_ >= 0.0)

  def toSampler(gmm: GMM, rng: Random) = {
    val distribution = EMGMM.toDistribution(gmm, rng)

    def sample() = {
      val x = distribution.sample()
      (x.toVector, Lazy(distribution.density(x)))
    }
    new tool.RejectionSampler(sample _, EMPPSE.acceptPoint)
  }
}

case class EMPPSE(
    lambda: Int,
    phenotype: Vector[Double] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    continuous: Vector[C],
    components: Int = 20,
    iterations: Int = 10,
    tolerance: Double = 0.0001,
    lowest: Int = 100,
    warmupSampler: Int = 1000)

object PPSE2Operations {

  import EMPPSE.ProbabilityMap

  def breeding[S, I, G](
    continuous: Vector[C],
    buildGenome: Vector[Double] => G,
    lambda: Int,
    //reject: Option[G => Boolean],
    gmm: S => Option[(GMM, RejectionSampler.State)]
 ): Breeding[S, I, G] =
    (s, population, rng) => {

      def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = Vector.fill(genomeLength)(() => rng.nextDouble()).map(_())

      gmm(s) match {
        case None => (0 to lambda).map(_ => buildGenome(randomUnscaledContinuousValues(continuous.size, rng))).toVector
        case Some(gmm) =>
          val sampler = EMPPSE.toSampler(gmm._1, rng)



          sampler.sampleVector(lambda, gmm._2)._2.
            map(_._1).
            map(g => buildGenome(g))
      }
    }

    def elitism[S, I, P: CanBeNaN](
      continuous: Vector[C],
      values: I => Vector[Double],
      phenotype: I => P,
      pattern: P => Vector[Int],
      probabilities: monocle.Lens[S, ProbabilityMap],
      hitmap: monocle.Lens[S, HitMap],
      gmm: monocle.Lens[S, Option[(GMM, RejectionSampler.State)]],
      components: Int,
      iterations: Int,
      tolerance: Double,
      lowest: Int,
      warmupSampler: Int): Elitism[S, I] = { (state, population, candidates, rng) =>

      def noNan = filterNaN(candidates, phenotype)
      def keepFirst(i: Vector[I]) = Vector(i.head)
      val population2 = keepNiches(phenotype andThen pattern, keepFirst)(population ++ noNan)

      if(population2.size >= lowest) {

        gmm.get(state) match {
          case None =>
            // TODO count hits and update densities

            def lowestHitIndividual = population2.take(lowest)

            val (gmmValue, _) =
              EMGMM.initializeAndFit(
                components = components,
                iterations = iterations,
                tolerance = tolerance,
                x = lowestHitIndividual.map(values).map(_.toArray).toArray,
                columns = continuous.size,
                rng
              )


            def state2 = (
              gmm.set(Some((gmmValue, EMPPSE.toSampler(gmmValue, rng).warmup(warmupSampler)))))(state)

            (state2, population2)
          case Some(gmmValue) =>

            val hm2 = addHits(phenotype andThen pattern, noNan, hitmap.get(state))
            val sortedPopulation2 = population2.sortBy(i => hm2.getOrElse(pattern(phenotype(i)), 1))

            def lowestHitIndividual = sortedPopulation2.take(lowest)

            val distribution = EMGMM.toDistribution(gmmValue._1, rng)

            def densities =
              noNan.groupBy { i => (phenotype andThen pattern)(i) }.view.
                mapValues { v => v.map(values).map(p => distribution.density(p.toArray))}.toSeq

            val hm = hitmap.get(state)
            val pm: ProbabilityMap = probabilities.get(state)

            def probabilityUpdate(p: (Vector[Int], Seq[Double])) = {
              val (pattern, densities) = p
              val hits = hm.getOrElse(pattern, 0)
              val newDensity = (pm.getOrElse(pattern, 0.0) * hits + densities.sum) / (hits + densities.size)
              pattern -> newDensity
            }

            def pm2 = pm ++ densities.map(probabilityUpdate)

            val (gmmValue2, _) = {
              try {
//                EMGMM.fit(
//                  means = gmmValue.means,
//                  covariances = gmmValue.covariances,
//                  weights = gmmValue.weights,
//                  components = components,
//                  iterations = iterations,
//                  tolerance = tolerance,
//                  x = lowestHitIndividual.map(values).map(_.toArray).toArray
//                )

                EMGMM.initializeAndFit(
                  components = components,
                  iterations = iterations,
                  tolerance = tolerance,
                  x = lowestHitIndividual.map(values).map(_.toArray).toArray,
                  columns = continuous.size,
                  rng
                )

              } catch {
                case t: org.apache.commons.math3.linear.SingularMatrixException =>
                  throw new RuntimeException("Computing GMM for points [" + lowestHitIndividual.map(values).map(p => s"""[${p.mkString(", ")}]""").mkString(", ") + "]", t)
              }
            }

            def state2 =
              (gmm.set(Some((gmmValue2, EMPPSE.toSampler(gmmValue2, rng).warmup(warmupSampler)))) andThen
                probabilities.set(pm2) andThen
                hitmap.set(hm2))(state)

            (state2, sortedPopulation2)
        }

      } else (state, population2)
    }


}

