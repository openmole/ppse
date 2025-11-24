package ppse.categorial

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


def crossover(i1: Individual, i2: Individual) = ???


def breeding(population: Vector[Individual], hitMap: HitMap, size: Int, random: Random) =
  // TODO biais towards lower hits
  val weights = Vector.fill(population.length)(1.0 / population.length)
  ???


