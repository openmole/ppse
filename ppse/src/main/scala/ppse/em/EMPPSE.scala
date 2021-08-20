package ppse.em

import breeze.linalg.DenseVector
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

import monocle._
import monocle.syntax.all._

import scala.language.higherKinds

object EMPPSE {

  type DensityMap = Map[Vector[Int], Double]
  case class EMPPSEState(
    hitmap: HitMap = Map(),
    gmm: Option[(GMM, RejectionSampler.State)] = None,
    probabilityMap: DensityMap = Map())

  case class Result[P](continuous: Vector[Double], pattern: Vector[Int], density: Double, phenotype: Vector[Double], individual: Individual[P])

  def result[P](population: Vector[Individual[P]], state: EvolutionState[EMPPSEState], continuous: Vector[C], phenotype: P => Vector[Double], pattern: Vector[Double] ⇒ Vector[Int]) = {
    val densityMap = state.focus(_.s) andThen Focus[EMPPSEState](_.probabilityMap) get
    val total = densityMap.map(_._2).sum

    population.map { i =>
      val ph = phenotype(i.phenotype)
      val pa = pattern(ph)

      Result(
        scaleContinuousValues(i.genome.toVector, continuous),
        pa,
        densityMap.getOrElse(pa, 0.0) / total,
        ph,
        i)
    }
  }

  def result(pse: EMPPSE, population: Vector[Individual[Vector[Double]]], state: EvolutionState[EMPPSEState]): Vector[Result[Vector[Double]]] =
    result(population, state, pse.continuous, identity, pse.pattern)

  type Genome = Array[Double]

  @Lenses case class Individual[P](
    genome: Genome,
    phenotype: P)

  def buildIndividual[P](g: Genome, f: P) = Individual(g, f)
  //def vectorPhenotype[P] = Focus[Individual[P]](_.phenotype) andThen arrayToVectorIso[Double]

  def initialGenomes(number: Int, continuous: Vector[C]/*, reject: Option[Genome => Boolean]*/, rng: scala.util.Random) = {
    def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = Array.fill(genomeLength)(() => rng.nextDouble()).map(_())

    //val rejectValue = reject.getOrElse((_: Genome) => false)

    def generate(acc: List[Genome], n: Int): Vector[Genome] =
      if (n >= number) acc.toVector
      else {
        val g = randomUnscaledContinuousValues(continuous.length, rng)
//        if (rejectValue(g)) generate(acc, n)
//        else generate(g :: acc, n + 1)
        generate(g :: acc, n + 1)
      }

    generate(List(), 0)
  }

  def breeding[P](
    continuous: Vector[C],
    lambda: Int): Breeding[EvolutionState[EMPPSEState], Individual[P], Genome] =
    PPSE2Operations.breeding(continuous, identity, lambda, Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.gmm) get)

  def elitism[P: CanBeNaN](
    pattern: P => Vector[Int],
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    warmupSampler: Int) =
    PPSE2Operations.elitism[EvolutionState[EMPPSEState], Individual[P], P](
      values = (Individual.genome andThen arrayToVectorIso[Double]).get,
      phenotype = (_: Individual[P]).phenotype,
      pattern = pattern,
      densityMap = Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.probabilityMap),
      hitmap = Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.hitmap),
      gmm = Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.gmm),
      iterations = iterations,
      tolerance = tolerance,
      dilation = dilation,
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
  def expression[P](phenotype: Vector[Double] => P, continuous: Vector[C]): Genome => Individual[P] = (g: Genome) => {
    val sc = scaleContinuousValues(g.toVector, continuous)
    Individual(g, phenotype(sc))
  }
//  deterministic.expression[Genome, Vector[Double], Individual](
//      values(_, continuous),
//      buildIndividual,
//      phenotype)
//}


  //
//  def reject(pse: PPSE2) = NSGA2.reject(pse.reject, pse.continuous)

  implicit def isAlgorithm: Algorithm[EMPPSE, Individual[Vector[Double]], Genome, EvolutionState[EMPPSEState]] = new Algorithm[EMPPSE, Individual[Vector[Double]], Genome, EvolutionState[EMPPSEState]] {
    def initialState(t: EMPPSE, rng: util.Random) = EvolutionState[EMPPSEState](s = EMPPSEState())

    override def initialPopulation(t: EMPPSE, rng: scala.util.Random) =
      deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
        EMPPSE.initialGenomes(t.lambda, t.continuous, rng),
        EMPPSE.expression(t.phenotype, t.continuous))

    def step(t: EMPPSE) =
      (s, pop, rng) =>
        deterministic.step[EvolutionState[EMPPSEState], Individual[Vector[Double]], Genome](
          EMPPSE.breeding(t.continuous, t.lambda),
          EMPPSE.expression[Vector[Double]](t.phenotype, t.continuous),
          EMPPSE.elitism(t.pattern, t.iterations, t.tolerance, t.dilation, t.warmupSampler),
          Focus[EvolutionState[EMPPSEState]](_.generation),
          Focus[EvolutionState[EMPPSEState]](_.evaluated)
        )(s, pop, rng)

  }

  def acceptPoint(x: Vector[Double]) =
    x.forall(_ <= 1.0) && x.forall(_ >= 0.0)

  def toSampler(gmm: GMM, rng: Random) = {
    val distribution = WDFEMGMM.toDistribution(gmm, rng)

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
  iterations: Int = 100,
  tolerance: Double = 0.0001,
  warmupSampler: Int = 1000,
  dilation: Double = 1.0)

object PPSE2Operations {

  import EMPPSE.DensityMap

  def breeding[S, I, G](
    continuous: Vector[C],
    buildGenome: Array[Double] => G,
    lambda: Int,
    //reject: Option[G => Boolean],
    gmm: S => Option[(GMM, RejectionSampler.State)]
  ): Breeding[S, I, G] =
    (s, population, rng) => {

      def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = Array.fill(genomeLength)(() => rng.nextDouble()).map(_())

      gmm(s) match {
        case None => (0 to lambda).map(_ => buildGenome(randomUnscaledContinuousValues(continuous.size, rng))).toVector
        case Some(gmm) =>

          val sampler = EMPPSE.toSampler(gmm._1, rng)

          sampler.sampleVector(lambda, gmm._2)._2.
            map(_._1.toArray).
            map(g => buildGenome(g))
      }
    }

    def elitism[S, I, P: CanBeNaN](
      values: I => Vector[Double],
      phenotype: I => P,
      pattern: P => Vector[Int],
      densityMap: monocle.Lens[S, DensityMap],
      hitmap: monocle.Lens[S, HitMap],
      gmm: monocle.Lens[S, Option[(GMM, RejectionSampler.State)]],
      iterations: Int,
      tolerance: Double,
      dilation: Double,
      warmupSampler: Int): Elitism[S, I] = { (state, population, candidates, rng) =>

      def offSpringWithNoNan = filterNaN(candidates, phenotype)
      def keepFirst(i: Vector[I]) = Vector(i.head)

      val newPopulation = keepNiches(phenotype andThen pattern, keepFirst)(population ++ offSpringWithNoNan)
      val hm2 = addHits(phenotype andThen pattern, offSpringWithNoNan, hitmap.get(state))

      def hits(i: I) = hm2.get(phenotype andThen pattern apply i)
      def weights(pop: Vector[I]) = {
        val w = pop.map(i => hits(i).getOrElse(1))
        w.map(h => 1.0 / h) // wMax - h + 1) //.map(math.pow(_, 2))
      }

      // TODO: Consider density in boostraping steps ?
      gmm.get(state) match {
        case None if newPopulation.size < 10 =>
          def state2 = hitmap.replace(hm2) apply (state)
          (state2, newPopulation)
        case None =>
          val (gmmValue, _) =
            WDFEMGMM.initializeAndFit(
              iterations = iterations,
              tolerance = tolerance,
              x = WDFEMGMM.toDenseMatrix(newPopulation.map(values).map(_.toArray).toArray),
              dataWeights = DenseVector(weights(newPopulation): _*),
              random = rng,
              retry = 0
            )

          val dilatedGMMValue = WDFEMGMM.dilate(gmmValue, dilation)

          def state2 =
            gmm.replace(Some((dilatedGMMValue, EMPPSE.toSampler(dilatedGMMValue, rng).warmup(warmupSampler)))) andThen
              hitmap.replace(hm2) apply (state)

          (state2, newPopulation)

        case Some(gmmValue) =>
          val distribution = WDFEMGMM.toDistribution(gmmValue._1, rng)

          def offspringDensities =
            offSpringWithNoNan.groupBy { i => (phenotype andThen pattern)(i) }.view.
              mapValues { v => v.map(values).map(p => 1 / distribution.density(p.toArray))}.toSeq

          val oldDensityMap: DensityMap = densityMap.get(state)

          def probabilityUpdate(p: (Vector[Int], Seq[Double])) = {
            val (pattern, densities) = p
            val newDensity = oldDensityMap.getOrElse(pattern, 0.0) + densities.sum
            pattern -> newDensity
          }

          def newDensityMap = oldDensityMap ++ offspringDensities.map(probabilityUpdate)

          //FIXME take to parameter
          def bestIndividualsOfPopulation = newPopulation //.sortBy(hits)

          val (gmmValue2, _) =
            WDFEMGMM.initializeAndFit(
              iterations = iterations,
              tolerance = tolerance,
              x = WDFEMGMM.toDenseMatrix(bestIndividualsOfPopulation.map(values).map(_.toArray).toArray),
              dataWeights = DenseVector(weights(bestIndividualsOfPopulation): _*),
              random = rng,
              retry = 0
            )

          val dilatedGMMValue = EMGMM.dilate(gmmValue2, dilation)

          def state2 =
            (gmm.replace(Some((dilatedGMMValue, EMPPSE.toSampler(dilatedGMMValue, rng).warmup(warmupSampler)))) andThen
              densityMap.replace(newDensityMap) andThen
              hitmap.replace(hm2))(state)

          (state2, newPopulation)
      }


    }


}

