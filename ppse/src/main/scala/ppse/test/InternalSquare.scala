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

object InternalSquare:
  def inCentralSquare(square: InternalSquare, point: Vector[Double]) =
    point.forall(c => c > 0.5 - square.s / 2 && c < 0.5 + square.s / 2 )
  //def nearFromBorders(dilatedSquares: DilatedSquares, point: Vector[Double]) =

  def dilateExtern(square: InternalSquare, point: Vector[Double]) =
    def dilateAxe(c: Double) =
      val b = if c > 0.5 then 1.0 else 0
      (c - b) * square.d + b

    point.map(dilateAxe)

  def dilateIntern(square: InternalSquare, point: Vector[Double]) =
    val f = (0.5 - square.s / 2) * square.d
    val z = (1 - 2 * f) / square.s

    def dilateAxe(c: Double) =
      (c - 0.5) * z + 0.5

    point.map(dilateAxe)

  def dilate(square: InternalSquare, point: Vector[Double]) =
    if inCentralSquare(square, point)
    then dilateIntern(square, point)
    else dilateExtern(square, point)

case class InternalSquare(s: Double, d: Double)
