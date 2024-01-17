package flocking

import scala.math._
import scala.collection._
import scala.util.Random
import java.awt.Color

import flocking.datatypes._
import flocking.tools._

trait ModelReynolds {
//  def worldWidth: Double
//  def worldHeight: Double
//  def envDivsHorizontal: Int
//  def envDivsVertical: Int
//  def populationSize: Int
//  def vision: Double
//  def visionObstacle: Double
//  def minimumSeparation: Double
//  def maxAlignTurn: Angle
//  def maxCohereTurn: Angle
//  def maxSeparateTurn: Angle
//  def stepSize: Double
//  def emptySpace: Int = new Color(0,0,0).getRGB()
//
//  lazy val env = Environment.empty(emptySpace, envDivsHorizontal, envDivsVertical, worldWidth, worldHeight)
//
//  def randomBird: Bird = Bird(Position(Random.nextDouble() * worldWidth, Random.nextDouble() * worldHeight),
//                              Heading.fromDouble(Random.nextDouble() * 2*Pi))
//  def randomInit: GraphBirds = GraphBirds((1 to populationSize).map( _ => randomBird ))
//
//  def start(iterations: Int): GraphBirds = run(iterations, randomInit)
//
//  def run(iterations: Int, g: GraphBirds): GraphBirds =
//    if (iterations <= 0) g
//    else run(iterations - 1, oneStep(g))
//
//  def oneStep(g: GraphBirds): GraphBirds = buildGraph(updateBirds(g))
//
//  def forEachState[T](maxiter: Int, f: (Model, Int, GraphBirds) => T): Seq[T] = forEachState(0, maxiter, f, randomInit)
//  def forEachState[T](i: Int, maxiter: Int, f: (Model, Int, GraphBirds) => T, state: GraphBirds): List[T] =
//    f(this, i, state) :: (if (i < maxiter) forEachState(i + 1, maxiter, f, oneStep(state)) else List())
//
//  def updateBirds(graph: GraphBirds): Seq[Bird] =
//    (0 until graph.birds.size).map((b: Int) => graph.birds(b).update(
//      graph.flockmates(b).map((i:Int) => (graph.birds(i), distanceBetween(graph.birds(i).position, graph.birds(b).position))),
//      graph.nearestNeighbour(b).map((i:Int) => (graph.birds(i), distanceBetween(graph.birds(i).position, graph.birds(b).position))),
//      this))
//
//  def buildGraph(birds: Seq[Bird]): GraphBirds = GraphBirds(birds)
//
//  object Bird {
//    def apply(_position: Point, _heading: Heading) = new Bird {
//      val position: Point = _position
//      val heading: Heading = _heading
//    }
//  }
//
//  object GraphBirds {
//    def apply(birds: Seq[Bird]) = flocking.GraphBirds(birds, vision, distanceBetween)
//  }
//
//  def distanceBetween(p1: Point, p2: Point): Double = Distance.torus(worldWidth,worldHeight)(p1,p2)
//
//  object Position {
//      val pChecker = PointBoundsKeeper(Point(0,0), Point(worldWidth, worldHeight))
//      def apply(x: Double, y: Double): Point = {
//        val pChecked = pChecker(Point(x,y))
//        Point(pChecked.x, pChecked.y)
//      }
//  }
}

//class ModelIterator(val model: Model) {
//  var currentState = model.randomInit
//  def step = {
//    currentState = model.oneStep(currentState)
//  }
//}
