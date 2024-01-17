
package flocking

import scala.math._
import scala.util.Random
import flocking.tools._
import datatypes._

object Model:

  def torusDistance(width: Double, height: Double)(p1: Point, p2: Point): Double = sqrt(pow(min(abs(p2.x - p1.x), width - abs(p2.x - p1.x)), 2) + pow(min(abs(p2.y - p1.y), height - abs(p2.y - p1.y)), 2))
  def distanceBetween(width: Double, height: Double, p1: Point, p2: Point): Double = torusDistance(width, height)(p1,p2)
  def updateBirds(model: Model, graph: GraphBirds): IArray[Bird] = graph.birds.map(_.update(graph, model))
  def buildGraph(model: Model, birds: IArray[Bird]): GraphBirds = GraphBirds(birds, model.vision, distanceBetween(model, _, _))
  def distanceBetween(model: Model, p1: Point, p2: Point): Double = torusDistance(model.worldWidth, model.worldHeight)(p1, p2)

  def run(model: Model, iterations: Int, g: GraphBirds): GraphBirds =
    if (iterations <= 0) g
    else run(model, iterations - 1, oneStep(model, g))

  def oneStep(model: Model, g: GraphBirds): GraphBirds = Model.buildGraph(model, Model.updateBirds(model, g))

  def randomInit(model: Model, environment: Environment, random: Random): GraphBirds =
    def randomBird(id: Int): Bird =
      Bird(
        id,
        Position(model.worldHeight, model.worldWidth, random.nextDouble() * model.worldWidth, random.nextDouble() * model.worldHeight),
        Heading(random.nextDouble() * 2*Pi),
        environment,
        model.vision // vision obstacles
      )

    val birds =
      IArray.tabulate(model.populationSize): id =>
        randomBird(id)

    GraphBirds(
      birds,
      model.vision,
      distanceBetween(model.worldWidth, model.worldHeight, _, _)
    )


  def start(model: Model, environment: Environment, iterations: Int, random: Random) = run(model, iterations, randomInit(model, environment, random))


case class Model(
  worldWidth: Double,
  worldHeight: Double,
  populationSize: Int,
  vision: Double,
  minimumSeparation: Double,
  maxAlignTurn: Angle,
  maxCohereTurn: Angle,
  maxSeparateTurn: Angle,
  stepSize: Double)








