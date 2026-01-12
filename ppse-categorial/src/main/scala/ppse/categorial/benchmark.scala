package ppse.categorial

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

import better.files.*

object benchmark:

  @main def ppseSquare(result: String) =
    import ppse.paper.benchmark.*

    val resultFile = File(result)
    val genomeSize = 2
    val lambda = 100
    val random = util.Random(42)

    val maxRareSample = 10

    val allPatterns = PatternSquare.allPatterns2D(PatternSquare.benchmarkPattern)

    ppse.categorial.evolution(
      genomeSize = genomeSize,
      lambda = lambda,
      generations = 1000,
      pattern = x => PatternSquare.pattern(PatternSquare.benchmarkPattern, x.toVector),
      random = random,
      trace =
        Some: i =>
          val all = allPatterns.filterNot(PatternSquare.isFallbackPattern)
          val resultSet = i.population.map(_.pattern).toSet
          val missed = all.size - resultSet.count(all.contains)
          resultFile.append(s"${i.generation*lambda},${i.population.size},$missed\n")
          println(s"${i.generation}: ${i.population.size}")
    )
