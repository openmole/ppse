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


import plotly.*
import plotly.Plotly.*
import plotly.element.*
import plotly.layout.*
import scala.util.Random
import better.files._
import mgo.evolution.niche.boundedGrid
import ppse.test.BenchmarkUniform.uniform2D
import scopt.OParser


//@main def visuSquare =
//
//  val square = Square(0.005)
//  def patternToSpace(pattern: Vector[Int]) =
//    def toSpace(x: Int) = x * 1.0 / 50.0
//
//    pattern.map(p => (toSpace(p), toSpace(p + 1)))
//
//  val poisson: Seq[Seq[Double]] =
//    (0 until 50).map { x =>
//      (0 until 50).map { y =>
//        val Vector((minX, maxX), (minY, maxY)) = patternToSpace(Vector(x, y))
//        if Square.inCentralSquare(square, Vector(minX, minY))
//        then 1.0
//        else 0.0
//
//        //DoublePoisson.inverse(minX, maxX, minY, maxY)
//      }
//    }
//
//  VisuSquare.displayMap(poisson)
//  val labels = Seq("Banana", "Banano", "Grapefruit")
//  val valuesA = labels.map(_ => util.Random.nextGaussian())
//  val valuesB = labels.map(_ => 0.5 + util.Random.nextGaussian())
//
//  Seq(
//    Bar(labels, valuesA, name = "A"),
//    Bar(labels, valuesB, name = "B")
//  )
  // demo source start
object VisuSquare:
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


  type Pattern = Vector[Int]
  type DensityMap = Map[Pattern, Double]

  def uniform2D(pattern: Vector[Double] => Vector[Int], points: Int, random: Random = new Random(42)) = {
    val drawn = (0 until points).map(_ => Vector.fill(2)(random.nextDouble()))

    val patterns =
      drawn.
        groupBy(p => pattern(p)).
        view.
        mapValues(_.size / points.toDouble).
        toMap

    patterns
    //(patterns, drawn.map(d => d -> patterns(pattern(d))))
  }

  def uniformOutput(f: Vector[Double] => Vector[Double], pattern: Vector[Double] => Vector[Int], points: Int, random: Random = new Random(42), dimension: Int = 2) = {
    val drawn = (0 until points).map(_ => Vector.fill(dimension)(random.nextDouble()))

    val patterns =
      drawn.
        groupBy(p => pattern(f(p))).
        view.
        mapValues(_.size / points.toDouble).
        toMap

    drawn.map(f).map(f => f -> patterns(pattern(f)))
  }


@main def visuSquare =

  val square = ExternalSquare(0.1)
  println(ExternalSquare.dilate(square, Vector(0.1, 0.5)))

//  def pattern(x: Vector[Double]) =
//    boundedGrid(
//      lowBound = Vector(0.0, 0.0),
//      highBound = Vector(1.0, 1.0),
//      definition = Vector(50, 50))(Square.dilate(square, x))
//


  def pattern(x: Vector[Double]) = ExternalSquare.dilate(square, x).map(_ * 50).map(_.toInt)


  println(pattern(Vector(0.49, 0.49)))

  def patternToSpace(pattern: Vector[Int]) =
    def toSpace(x: Int) = x * 1.0 / 50.0
    pattern.map(p => (toSpace(p), toSpace(p + 1)))

//        def referenceDensity(p: Vector[Int]) =
//          val Vector((minX, maxX), (minY, maxY)) = patternToSpace(p)
//          DoublePoisson.inverse(minX, maxX, minY, maxY)


  val p = BenchmarkUniform.uniform2D(pattern, 500000)
  val indexPattern = p.groupMap(_._1)(_._2).view.mapValues(_.head).toMap

  val map =
    (0 until 50) map { x =>
      (0 until 50) map { y =>
        indexPattern.getOrElse(Vector(x, y), 0.0)
      }
    }

  VisuSquare.displayMap(map)
//          val allPatterns =
//            for
//              x <- 0 until 50
//              y <- 0 until 50
//            yield Vector(x, y)
//
//          allPatterns.map { p =>
//            val density = indexPattern.getOrElse(p, 0.0)
//            density
//          }

//        for
//          points <- 1000 to 50000 by 1000
//        do
//          val p = BenchmarkUniform.uniform2D(pattern, points)
//
//          val indexPattern = p.groupMap(_._1)(_._2).view.mapValues(_.head).toMap
//
//          val error =
//            allPatterns.map { p =>
//              val density = indexPattern.getOrElse(p, 0.0)
//              math.abs(referenceDensity(p) - density)
//            }.sum
//
//          f.appendLine(s"$points, ${error}")



