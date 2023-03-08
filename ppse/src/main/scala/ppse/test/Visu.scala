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


import plotly._, element._, layout._, Plotly._

@main def visu =
  def patternToSpace(pattern: Vector[Int]) =
    def toSpace(x: Int) = x * 1.0 / 50.0

    pattern.map(p => (toSpace(p), toSpace(p + 1)))

  val poisson: Seq[Seq[Double]] =
    (0 until 50).map { x =>
      (0 until 50).map { y =>
        val Vector((minX, maxX), (minY, maxY)) = patternToSpace(Vector(x, y))
        DoublePoisson.inverse(minX, maxX, minY, maxY)
      }
    }

  displayMap(poisson)
//  val labels = Seq("Banana", "Banano", "Grapefruit")
//  val valuesA = labels.map(_ => util.Random.nextGaussian())
//  val valuesB = labels.map(_ => 0.5 + util.Random.nextGaussian())
//
//  Seq(
//    Bar(labels, valuesA, name = "A"),
//    Bar(labels, valuesB, name = "B")
//  )
  // demo source start

def displayMap(p: Seq[Seq[Double]]) =

//  val trace2 = Scatter(p.map(_(0)), p.map(_(1)))
//    .withMode(ScatterMode(ScatterMode.Markers))
//    .withMarker(
//      Marker()
//        .withColor(Color.RGBA(204, 204, 204, 0.95))
//        .withLine(
//          Line()
//            .withColor(Color.RGBA(217, 217, 217, 1.0))
//            .withWidth(1.0)
//        )
//        .withSymbol(Symbol.Circle())
//        .withSize(16)
//    )
  val data = Seq(
    Heatmap()
      .withZ(
        p
      )
      .withX(Seq(0.0, 1.0))
  )

  data.plot(
    width = 800,
    height = 800,
    title = "Level"
  )

