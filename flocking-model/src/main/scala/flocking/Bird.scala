package flocking

import flocking.datatypes._
import scala.collection._
import scala.math._

import datatypes._

case class Bird(id: Int, position: Point, heading: Heading, environment: Environment, visionObstacle: Double):

  def update(g: GraphBirds, model: Model): Bird =
    val flockmates = g.flockmates(id).map(i => (g.birds(i), Model.distanceBetween(model, g.birds(i).position, g.birds(id).position)))
    val nearestNeighbour = g.nearestNeighbour(id).map(i => (g.birds(i), Model.distanceBetween(model, g.birds(i).position, g.birds(id).position)))

    val newHeading =
      updateHeading(
        flockmates.map(_._1),
        nearestNeighbour,
        model.minimumSeparation,model.maxSeparateTurn,
        model.maxAlignTurn,model.maxCohereTurn,
        environment,
        visionObstacle)

    val newPosition =
      Position(
        model.worldWidth,
        model.worldHeight,
        position.x + model.stepSize * cos(newHeading.toDouble),
        position.y + model.stepSize * sin(newHeading.toDouble)
      )

    Bird(id, newPosition, newHeading, environment, visionObstacle)

  /** Bird orientation, returns the new heading of the bird
    */
  def updateHeading(
    flockmates: Seq[Bird],
    nearestNeighbourAndDist: Option[(Bird, Double)],
    minimumSeparation: Double,
    maxSeparateTurn: Angle,
    maxAlignTurn: Angle,
    maxCohereTurn: Angle,
    env: Environment,
    visionObstacle: Double): Heading =
    //   find-flockmates
    //   if any? flockmates
    //     [ find-nearest-neighbor
    //       ifelse distance nearest-neighbor < minimum-separation
    //         [ separate ]
    //         [ align
    //           cohere ] ]
    heading +
      (nearestNeighbourAndDist match {
        case None => Angle(0)
        case Some((nearestNeighbour: Bird, nnDistance: Double)) =>
          if (nnDistance < minimumSeparation) separate(nearestNeighbour.heading, maxSeparateTurn)
          else alignAndCohere(flockmates, maxAlignTurn, maxCohereTurn)}) +
        (if onObstacle(env)
        then getOutOfObstacle(env, visionObstacle)
        else turnAwayFromObstacles(env, visionObstacle))

  def separate(nearestNeighbourHeading: Heading, maxSeparateTurn: Angle): Angle =
    // turn-away ([heading] of nearest-neighbor) max-separate-turn
    val angle: Angle = heading angleTo nearestNeighbourHeading
    val newHeading: Heading = heading - angle
    turnAtMost(newHeading, maxSeparateTurn)

  def alignAndCohere(flockmates: Seq[Bird], maxAlignTurn: Angle, maxCohereTurn: Angle): Angle =
    // turn-towards average-flockmate-heading max-align-turn
    // turn-towards average-heading-towards-flockmates max-cohere-turn
     turnAtMost(averageFlockmateHeading(flockmates), maxAlignTurn) + turnAtMost(averageHeadingTowardsFlockmates(flockmates), maxCohereTurn)

  val sensorsAngles = IArray(
    Angle(-Pi/12), Angle(Pi/12),
    Angle(-Pi/10), Angle(Pi/10),
    Angle(-Pi/8), Angle(Pi/8),
    Angle(-Pi/6), Angle(Pi/6),
    Angle(-Pi/4), Angle(Pi/4),
    Angle(-Pi/3), Angle(Pi/3)
  )

  def onObstacle(env:Environment): Boolean = !Environment.isEmpty(env, position.x, position.y)

  def getOutOfObstacle(env: Environment, vision: Double): Angle =
    val detectedFreeSpace: Option[Angle] = sensorsAngles.find(!obstacleAt(env, _, vision))
    detectedFreeSpace match
      case None => Angle(0)
      case Some(a:Angle) => a


  def turnAwayFromObstacles(env: Environment, vision: Double): Angle =
    val detectedObstacle: Option[Angle] = sensorsAngles.find(obstacleAt(env, _, vision))
    detectedObstacle match
      case None => Angle(0)
      case Some(Angle(x)) =>
        if (x >= 0) Angle(x  - Pi / 2)
        else Angle(x + Pi / 2)

  def obstacleAt(env: Environment, angle: Angle, distance: Double): Boolean =
    val sensorTheta = (heading + angle).toDouble
    !Environment.isEmpty(env, position.x + distance * cos(sensorTheta), position.y + distance * sin(sensorTheta))

  def averageFlockmateHeading(flockmates: Seq[Bird]): Heading =
    // ;; We can't just average the heading variables here.
    // ;; For example, the average of 1 and 359 should be 0,
    // ;; not 180.  So we have to use trigonometry.
    // let x-component sum [dx] of flockmates
    // let y-component sum [dy] of flockmates
    // ifelse x-component = 0 and y-component = 0
    //   [ report heading ]
    //   [ report atan x-component y-component ]
    val xComponent: Double = flockmates.map((b:Bird) => cos(b.heading.toDouble)).sum
    val yComponent: Double = flockmates.map((b:Bird) => sin(b.heading.toDouble)).sum
    if (xComponent == 0 && yComponent == 0) heading
    else Heading.fromDouble(atan2(yComponent, xComponent))

  def averageHeadingTowardsFlockmates(flockmates: Seq[Bird]): Heading =
    // ;; "towards myself" gives us the heading from the other turtle
    // ;; to me, but we want the heading from me to the other turtle,
    // ;; so we add 180
    // let x-component mean [sin (towards myself + 180)] of flockmates
    // let y-component mean [cos (towards myself + 180)] of flockmates
    // ifelse x-component = 0 and y-component = 0
    //   [ report heading ]
    //   [ report atan x-component y-component ]
    val xComponent: Double = flockmates.map((b:Bird) => b.position.x - position.x).sum / flockmates.size
    val yComponent: Double = flockmates.map((b:Bird) => b.position.y - position.y).sum / flockmates.size
    if (xComponent == 0 && yComponent == 0) heading
    else Heading.fromDouble(atan2(yComponent, xComponent))

  def turnAtMost(newHeading: Heading, maxTurn: Angle): Angle =
    // ifelse abs turn > max-turn
    //   [ ifelse turn > 0
    //       [ rt max-turn ]
    //       [ lt max-turn ] ]
    //   [ rt turn ]
    val turn: Angle = heading angleTo newHeading
    if (turn.abs < maxTurn) turn
    else if (turn >= Angle(0)) maxTurn else -maxTurn

  override def toString = s"Bird(x=${position.x}, y=${position.y}, heading=$heading)"

