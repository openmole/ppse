package flocking

import flocking.datatypes.Point
import flocking.datatypes.*

object GraphBirds:
  def apply(birds: IArray[Bird], vision: Double, distance: (Point, Point) => Double): GraphBirds =

    val distanceMatrix: IArray[IArray[Double]] =
      IArray.tabulate(birds.length, birds.length): (i1, i2) =>
        if i1 == i2
        then 0.0
        else distance(birds(i1).position, birds(i2).position)

    val mates: IArray[IArray[Int]] =
      IArray.tabulate(birds.length): i1 =>
        IArray.unsafeFromArray:
          birds.indices.toArray.filter: i2 =>
            i1 != i2 && distanceMatrix(i1)(i2) <= vision

    val closestMate =
      birds.indices.map: i1 =>
        val ms = mates(i1)
        if ms.isEmpty
        then None
        else Some(ms.minBy(i2 => distanceMatrix(i1)(i2)))

    GraphBirds(
      birds = birds,
      flockmates = mates,
      nearestNeighbour = closestMate
    )



case class GraphBirds(birds: IArray[Bird], flockmates: IArray[IArray[Int]], nearestNeighbour: Seq[Option[Int]]):
  def areFlockmates(b1: Int, b2: Int) = flockmates(b1).contains(b2)
  override def toString = "Birds: " ++ birds.toString ++ "\nFlockmates: " ++ flockmates.toString ++ "\nNearestNeighbours: " ++ nearestNeighbour.toString

