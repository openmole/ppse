package ppse.categorial

import javax.swing.Popup
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

type Genome = Vector[Double]
type Pattern = Vector[Int]
type HitMap = Map[Vector[Int], Int]
case class Individual(genome: Genome, pattern: Pattern)
case class StepInfo(population: Vector[Individual], generation: Int)

def crossover(i1: Individual, i2: Individual) = ???

def breeding(population: Vector[Individual], genomeSize: Int, size: Int, maxRareSample: Int, hitMap: HitMap, random: Random): Vector[Genome] =
  def allAtMaxSample = population.forall(i => hitMap.getOrElse(i.pattern, 0) >= maxRareSample)

  if allAtMaxSample
  then Vector.fill(size, genomeSize)(random.nextDouble())
  else
    val weights =
      val w = population.map(p => hitMap(p.pattern)).map(p => math.log(1+ random.nextDouble) / p)
      val total = w.sum
      w.map(_ / total)
      
    ???


def updateHitMap(offspringPopulation: Vector[Individual], hitMap: HitMap): HitMap =
  val newMap = collection.mutable.Map[Vector[Int], Int]() ++ hitMap
  for
    i <- offspringPopulation
  do
    newMap.updateWith(i.pattern):
      case None => Some(1)
      case Some(v) => Some(v + 1)

  newMap.toMap


def elitism(population: Vector[Individual], offspringPopulation: Vector[Individual]): Vector[Individual] =
  (population ++ offspringPopulation)
    .groupBy(_.pattern)
    .toVector
    .map(_._2.head)


def evolution(
  genomeSize: Int,
  lambda: Int,
  generations: Int,
  pattern: Vector[Double] => Vector[Int],
  population: Vector[Individual] = Vector(),
  hitMap: HitMap = Map(),
  generation: Int = 0,
  maxRareSample: Int = 10,
  trace: Option[StepInfo => Unit] = None,
  random: Random): Vector[Individual] =

  trace.foreach(f => f(StepInfo(population, generation)))

  if generation >= generations
  then population
  else
    val offspringGenome = breeding(population, genomeSize, lambda, maxRareSample, hitMap, random)
    val offspringPopulation = offspringGenome.map(g => Individual(g, pattern(g)))
    val newPopulation = elitism(population, offspringPopulation)
    val newHitMap = updateHitMap(offspringPopulation, hitMap)
    evolution(genomeSize, lambda, generations, pattern, newPopulation, newHitMap, generation + 1, maxRareSample, trace, random)
