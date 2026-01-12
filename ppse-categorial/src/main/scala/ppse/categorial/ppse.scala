package ppse.categorial

import org.apache.commons.math3.distribution.{EnumeratedDistribution, MixtureMultivariateNormalDistribution, MultivariateNormalDistribution}
import org.apache.commons.math3.util.{MathArrays, Pair}
import org.apache.commons.math3.linear.DiagonalMatrix
import org.apache.commons.math3.linear.MatrixUtils

import java.util
import scala.util.Random
import scala.jdk.CollectionConverters.*
import ppse.paper.tool
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
case class Individual(genome: Genome, pattern: Pattern)


def crossover(i1: Individual, i2: Individual) = ???


def breeding(population: Vector[Individual], hitMap: HitMap, size: Int, random: Random) =
  // TODO biais towards lower hits
  val weights = Vector.fill(population.length)(1.0 / population.length)
  ???


/*
def choose(population: Vector[Genome], weights: Vector[Double], random: Random): (Genome, Double) =
  val randomValue = random.nextDouble()
  val probabilities = MathArrays.normalizeArray(weights.toArray, 1.0)
  val cumulativeProbabilities = probabilities.foldLeft(Array(0.0))((a,p)=> a.appended(a.last + p)).tail
  val index = {
    val r = util.Arrays.binarySearch(cumulativeProbabilities, randomValue)
    if r < 0 then -r - 1 else r
  }
  assert(index >=0 && index < probabilities.length && randomValue < cumulativeProbabilities(index))
  (population(index), probabilities(index))
*/

def mutate(population: Vector[Genome], genomeSize: Int, weights: Vector[Double], sigmaMax: Double, random: Random): Genome =
  val apacheRandom = tool.toApacheRandom(random)
  val comp = (population zip weights).map((p,w) => new Pair[Vector[Double], java.lang.Double](p._1,w)).toList.asJava
  val dist = new EnumeratedDistribution[Vector[Double]](apacheRandom, comp)
  // We generate a covariance matrix using an identity matrix multiplied by the sigma
  val matrix = MatrixUtils.createRealIdentityMatrix(genomeSize).scalarMultiply(sigmaMax).getData
  val list = population.zip(weights).map((g,w) => new Pair[java.lang.Double,MultivariateNormalDistribution](w, new MultivariateNormalDistribution(g._1.toArray,matrix))).toList.asJava
  val mixture = new MixtureMultivariateNormalDistribution(apacheRandom, list)
  val choice = mixture.sample()
  (choice.toVector, mixture.density(choice))

@main def mutateTest(): Unit =
  mutate(Vector((Vector(0),0.1),(Vector(1),0.5),(Vector(2),0.4)),1,Vector(0.1,0.5,0.4),0.1,Random(42))