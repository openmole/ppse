package ppse.test

import ppse.em.Clustering

import scala.util.Random

/*
 * Copyright (C) 2023 Romain Reuillon
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


@main def testClustering =
  val rng = new Random(42)

  val square =
    PatternSquare(
      PatternSquare.Square(Vector(0.5, 0.5), 0.01, 10),
      PatternSquare.Square(Vector(0.25, 0.25), 0.01, 10),
      PatternSquare.Square(Vector(0.25, 0.75), 0.01, 10),
      PatternSquare.Square(Vector(0.75, 0.25), 0.01, 10),
      PatternSquare.Square(Vector(0.75, 0.75), 0.01, 10)
    )

  def drawInSquare(s: PatternSquare.Square, rng: Random) =
    val (ox, oy) = (rng.nextDouble(), rng.nextDouble())
    def x = (ox * s.size) + (s.center(0) - s.size / 2.0)
    def y = (oy * s.size) + (s.center(1) - s.size / 2.0)
    Array(x, y)

  def draw(i: Int, points: Int, rng: Random) =
    (0 until points).map(_ => drawInSquare(square.squares(i), rng))

  val points = draw(0, 80, rng) ++ (1 to 4).flatMap(i => draw(i, 3, rng))

  val clusters = Clustering.build(points.toArray, 2, Some(points.map(_ => 1.0).toArray))
  println("CLUSTERS")
  println(clusters._1.toSeq.map{p => "POINT("+p.toSeq.mkString(" ")+")"}.mkString("\n"))

