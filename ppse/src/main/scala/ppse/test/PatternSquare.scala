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

object PatternSquare:
  case class Square(center: Vector[Double], size: Double, grid: Int):
    def dimension = center.size

  def inSquare(square: Square, point: Vector[Double]) =
    (point zip square.center).forall { (c, sc) =>
      c >= sc - square.size / 2 && c < sc + square.size / 2
    }

//  def patternExtern(square: PatternSquare, point: Vector[Double]) =
//    grid(square, point)
//

  def patternIntern(square: Square, point: Vector[Double]) =
    def grid(size: Int, x: Vector[Double]) = x.map(_ * size).map(_.floor.toInt)
    val z = 1 / square.size
    grid(square.grid, (point zip square.center).map((c, sc) => (c - sc + square.size / 2) * z))

  def pattern(ps: PatternSquare, point: Vector[Double]) =
    (ps.squares.zipWithIndex).find((s, _) => inSquare(s, point)) match
      case None => Vector.fill(point.size + 1)(-1)
      case Some(s, i) => Vector(i) ++ patternIntern(s, point)

//  def patternDensity(square: PatternSquare, dim: Int = 2) =
//    math.pow(square.s, dim) / math.pow(square.g, dim)

  def patternDensity(ps: PatternSquare, p: Vector[Int]) =
    p.head match
      case -1 => patternDensityForRemaining(ps)
      case i => patternDensityForSquare(ps.squares(i))

  def patternDensityForSquare(square: Square) =
    math.pow(square.size, square.dimension) / math.pow(square.grid, square.dimension)

  def patternDensityForRemaining(patternSquare: PatternSquare) =
    def volume(square: Square) = math.pow(square.size, square.dimension)
    1.0 - patternSquare.squares.map(volume).sum

  def allPatterns2D(patternSquare: PatternSquare): Vector[Vector[Int]] =
    Vector(Vector(-1, -1, -1)) ++ {
      for
        (s, i) <- patternSquare.squares.zipWithIndex
        x <- 0 until s.grid
        y <- 0 until s.grid
      yield Vector(i, x, y)
    }

case class PatternSquare(squares: PatternSquare.Square*)
