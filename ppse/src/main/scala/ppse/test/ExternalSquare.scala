package ppse.test

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

object ExternalSquare:
  def inCentralSquare(square: ExternalSquare, point: Vector[Double]) =
    point.forall(c => c > 0.5 - square.s / 2 && c < 0.5 + square.s / 2 )
  //def nearFromBorders(dilatedSquares: DilatedSquares, point: Vector[Double]) =

  def dilateExtern(square: ExternalSquare, point: Vector[Double]) =
    def dilateAxe(c: Double) =
      val b = if c > 0.5 then 1.0 else 0
      (b - c) + b

    point.map(dilateAxe)

  def dilateIntern(square: ExternalSquare, point: Vector[Double]) =
    val z = 1 / square.s

    def dilateAxe(c: Double) = (c - 0.5) * z + 0.5

    point.map(dilateAxe)

  def dilate(square: ExternalSquare, point: Vector[Double]) =
    if inCentralSquare(square, point)
    then dilateIntern(square, point)
    else dilateExtern(square, point)

  def patternDensity(square: ExternalSquare, patterns: Int, dim: Int = 2) = math.pow(square.s, dim) / patterns

case class ExternalSquare(s: Double)
