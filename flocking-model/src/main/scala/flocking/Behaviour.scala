package flocking

/*
 * Copyright (C) 2021 Romain Reuillon
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

import java.util.Random

object Behaviour:

  def countGroups(gb: GraphBirds): Int = countGroups(gb, 0, (0 until gb.birds.size).toSet)

  def countGroups(gb: GraphBirds, nclustersFound: Int, remaining: Set[Int]): Int =
    if remaining.isEmpty
    then nclustersFound
    else countGroups(gb, nclustersFound + 1, remaining -- extractComponent(gb, remaining.head, Set()))

  def extractComponent(gb: GraphBirds, start: Int, visited: Set[Int]): Set[Int] =
    if gb.birds.isEmpty
    then Set()
    else
      val neighbours = gb.flockmates(start)
      if neighbours.isEmpty
      then Set(start)
      else
        neighbours.foldLeft(visited + start): (a, b) =>
          if !a.contains(b)
          then extractComponent(gb, b, a)
          else a

  def nearestNeighbour(d: DistMatrix)(i: Int, birds: Seq[Bird]): Int =
    birds.indices.minBy: j =>
      if i != j
      then d(i, j)
      else Double.MaxValue

  def voronoiNeighbours(birds: Seq[Bird], dm: DistMatrix): Seq[Seq[Int]] =
    val nnf = nearestNeighbour(dm)_
    val nn =
      for i <- birds.indices
      yield nnf(i, birds)

    for i <- birds.indices
    yield voronoiNeighbours(birds, nn, i)

  def voronoiNeighbours(birds: Seq[Bird], nearestNeigh: Seq[Int], i: Int): Seq[Int] =
    for
      j <- birds.indices
      if (i != j) && nearestNeigh(j) == i
    yield j

  def kNearestNeighbours(k: Int, birds:Seq[Bird], dm: DistMatrix): Seq[Seq[Int]] =

    def insert(x: Int, k: Int, nn: List[Int], distFromI: Int => Double): List[Int] =
      if k == 0
      then List()
      else
        if nn.isEmpty
        then List(x)
        else
          if distFromI(x) < distFromI(nn.head)
          then (x :: nn) take k
          else nn.head :: insert(x, k - 1, nn.tail, distFromI)

    def knn(i: Int): Seq[Int] =
      birds.indices.foldRight(List[Int]()): (j,nn) =>
        if j == i
        then nn
        else insert(j, k, nn, {dm(i,_)})

    birds.indices.map(knn)


  def distBetween(neighbours: Seq[Seq[Int]], dm: DistMatrix): Seq[Seq[Double]] =
    neighbours.indices.map((i: Int) => neighbours(i).map((j: Int) => dm(i,j)))

  def relativeDiffusion(
    neighboursDistAtT1: Seq[Seq[Double]],
    neighboursDistAtT2: Seq[Seq[Double]]): Double =

    def sumOver(is: Range, f: Int => Double): Double = (is map f).sum
    def averageOver(is: Range, f: Int => Double): Double = sumOver(is, f) / is.size.toDouble

    averageOver(
      neighboursDistAtT1.indices,
      i =>
        val ni = neighboursDistAtT1(i).size
        (1.0 / ni) *
          sumOver(
            neighboursDistAtT1(i).indices,
            j => 1 - (math.pow(neighboursDistAtT1(i)(j), 2) / math.pow(neighboursDistAtT2(i)(j), 2))
          )
    )

  def countGroups(model: Model, state: GraphBirds): Double =
    countGroups(state) / (model.populationSize.toDouble)

  def relativeDiffusion(model: Model, state1: GraphBirds, state2: GraphBirds): Double =
    val dist1 =
      val dm = DistMatrix(state1.birds.map(_.position), Model.distanceBetween(model, _, _))
      val neighbs = kNearestNeighbours(3, state1.birds, dm)
      distBetween(neighbs, dm)

    val dist2 =
      val dm = DistMatrix(state2.birds.map(_.position), Model.distanceBetween(model, _, _))
      val neighbs = kNearestNeighbours(3, state2.birds, dm)
      distBetween(neighbs, dm)

    relativeDiffusion(dist1, dist2)

  def velocity(model: Model, state1: GraphBirds, state2: GraphBirds): Double =
    val distances =
      (state1.birds.sortBy(_.id) zip state2.birds.sortBy(_.id)).map: (o, n) =>
        Model.distanceBetween(model, o.position, n.position)

    distances.sum / distances.size

  def computeBehaviour(
    model: Model,
    environment: Environment,
    random: Random,
    steps: Int = 500,
    s1: Int = 449,
    s2: Int = 499) =

    val states = Iterator.iterate(Model.randomInit(model, environment, random))(Model.oneStep(model, _)).take(steps).toArray
    val diffusion = relativeDiffusion(model, states(s1), states(s2))
    val velocity = Behaviour.velocity(model, states(s1), states(s2))
    val bigest = states(s2).flockmates.map(_.size).max.toDouble

    Seq(diffusion, velocity, bigest)


import datatypes.*

import java.util.Random
import scala.util.Random

case class DistMatrix(distances: Vector[Vector[Double]]):
  def apply(i: Int,j: Int): Double =
    if (i == j) 0
    else if (i < j) distances(i)(j - i - 1)
    else apply(j,i)


object DistMatrix:
  def apply(points: Seq[Point], distFunc: (Point, Point) => Double): DistMatrix =
    def distances: Vector[Vector[Double]] =
      (for {i <- 0 until (points.size - 1)} yield (for {j <- i + 1 until points.size} yield distFunc(points(i), points(j))).toVector).toVector

    DistMatrix(distances)

  def euclidean(p1: Point, p2: Point): Double = math.sqrt(math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y,2))


object BehaviourTest extends App:

//  val model =
//    Model(
//      worldWidth = 1,
//      worldHeight = 1,
//      populationSize = 200,
//      vision = 10 / 70.0,
//      minimumSeparation = 1 / 70.0,
//      maxAlignTurn = Angle(math.toRadians(5)),
//      maxCohereTurn = Angle(math.toRadians(3)),
//      maxSeparateTurn = Angle(math.toRadians(1.5)),
//      stepSize = 0.2 / 70.0
//    )

  val model =
    Model(
      worldWidth = 32,
      worldHeight = 32,
      populationSize = 128,
      vision = 10,
      minimumSeparation = 0.01,
      maxAlignTurn = Angle(math.toRadians(5)),
      maxCohereTurn = Angle(math.toRadians(3)),
      maxSeparateTurn = Angle(math.toRadians(1.5)),
      stepSize = 0.01
    )

  val environment = Environment.empty(model.worldWidth, model.worldHeight)

  println(Behaviour.computeBehaviour(model, environment, new java.util.Random(22)))
//  println(Behaviour.defaultDescription(model, environment, 1000, new java.util.Random(100)))



//
//def apply(_populationSize : Int,
//          _vision: Double,
//          _minimumSeparation: Double,
//          _stepSize: Double,
//          _maxAlignTurn: Double,
//          _maxCohereTurn: Double,
//          _maxSeparateTurn: Double
//         ) = {
//  new Behaviour {
//    val model = new Model {
//      val worldWidth: Double = 1
//      val worldHeight: Double = 1
//      val populationSize: Int = _populationSize
//      val vision: Double = _vision
//      val minimumSeparation: Double = _minimumSeparation
//      val maxAlignTurn: Angle = toAngle(_maxAlignTurn)
//      val maxCohereTurn: Angle = toAngle(_maxCohereTurn)
//      val maxSeparateTurn: Angle = toAngle(_maxSeparateTurn)
//      val stepSize: Double = _stepSize
//    }
//  }.defaultDescription
//}
