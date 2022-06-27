/*
 * Copyright (C) 2022 Romain Reuillon
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

package benchmark :

  def powDensity(base: Vector[Double], size: Vector[Double], p: Double = 4.0) =
    def antecedent(x: Double) = math.pow(x, 1 / p)
    (antecedent(base(0) + size(0)) - antecedent(base(0))) * (antecedent(base(1) + size(1)) - antecedent(base(1)))

  def powDensityMap(division: Vector[Int], p: Double = 4.0) =
    val sx = 1.0 / division(0)
    val sy = 1.0 / division(1)

    def densities =
      for
        (x, i) <- (BigDecimal(0.0) until 1.0 by sx).zipWithIndex
        (y, j) <- (BigDecimal(0.0) until 1.0 by sy).zipWithIndex
      yield
        Vector(i, j) -> powDensity(Vector(x, y).map(_.toDouble), Vector(sx, sy), p)

    densities.toMap

  @main def benchmark =
    import java.util.Random

    def pattern(x: Vector[Double], g: Vector[Int]): Vector[Int] =
      def pow(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 4.0))
      pow(x) zip g map { (f, g) => math.floor(f * g).toInt }

    val exactDensityMap = powDensityMap(Vector(50, 50))

    val genomeSize = 10
    val lambda = 100
    val dimension = 2
    val generations = 2000
    val dilation = 2.0
    val fitOnRarest = 100

    val divisions = Vector.fill(dimension)(50)

    val pdf = ppse.evolution(
      genomeSize = genomeSize,
      lambda = lambda,
      generations = generations,
      fitOnRarest = fitOnRarest,
      pattern = pattern(_, divisions),
      random = new Random(42))

    println(pdf.size)

    for (pattern, density) <- pdf
    do println(s"${pattern.mkString(",")},$density")

    val spreads =
      for (pattern, density) <- pdf
      yield
        val spread =
          val exact = exactDensityMap(pattern)
          math.abs(density - exact) / exact
        spread

    println(spreads.sum / spreads.size)