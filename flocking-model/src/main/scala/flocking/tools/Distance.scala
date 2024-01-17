package flocking.tools

import flocking.datatypes.Point

import scala.math.*
import scala.collection.*
import scala.util.Random
import flocking.datatypes.*

object Distance:
  def torus(width:Double, height:Double)(p1: Point, p2: Point): Double = sqrt(pow(min(abs(p2.x - p1.x), width - abs(p2.x - p1.x)), 2) + pow(min(abs(p2.y - p1.y), height - abs(p2.y - p1.y)),2))
  def euclidean(p1: Point, p2: Point): Double = sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y,2))

object DistMatrix:
  def apply(points: Seq[Point], distFunc: (Point, Point) => Double): DistMatrix =
    val matrix = (for {i <- 0 until (points.size - 1)} yield (for {j <- i+1 until points.size} yield distFunc(points(i), points(j))).toVector).toVector
    new DistMatrix(matrix)



class DistMatrix(distances: Vector[Vector[Double]]):
 def apply(i: Int,j: Int): Double =
    if (i == j) 0
    else if (i < j) distances(i)(j - i - 1)
    else apply(j, i)

