package flocking

import flocking.datatypes.Angle
import flocking.{Behaviour, Environment, Model}

import java.io.File
import scopt.*
import mgo.evolution.*
import mgo.evolution.niche.*
import ppse.em.*

import scala.concurrent.duration.Duration

def behaviour(rng: util.Random, parameters: Seq[Double]) =
  val model =
    Model(
      worldWidth = 32,
      worldHeight = 32,
      populationSize = 128,
      vision = parameters(0),
      minimumSeparation = parameters(1),
      maxAlignTurn = Angle(parameters(2)),
      maxCohereTurn = Angle(parameters(3)),
      maxSeparateTurn = Angle(parameters(4)),
      stepSize = 0.05
    )

  val environment = Environment.empty(model.worldWidth, model.worldHeight)

  Behaviour.computeBehaviour(model, environment, rng.self).toVector


//  def replications = 10

//  val behaviours =
//    for seed <- 0 until replications
//    yield Future(Behaviour.computeBehaviour(model, environment, new java.util.Random(seed)))
//
//  Await.result(Future.sequence(behaviours), Duration.Inf).transpose.map(_.sum / replications).toVector
//
//  val behaviours =
//    for seed <- 0 until 20
//      yield Behaviour.computeBehaviour(model, environment, new java.util.Random(seed))
//
//  behaviours.transpose.map(_.sum / 20.0).toVector

@main def flockingPPSE(args: String*) =

  scribe.Logger.root.withMinimumLevel(scribe.Level.Info).replace()

  case class Config(
    trace: Option[File] = None)

  val builder = OParser.builder[Config]

  val parser =
    import builder.*
    OParser.sequence(
      programName("flocking"),
      head("scopt", "4.x"),
      opt[String]('t', "trace").action((x, c) => c.copy(trace = Some(File(x))))
    )

  OParser.parse(parser, args, Config()) match
    case Some(config) =>
      val ppse = EMPPSE2(
        lambda = 10,
        phenotype = behaviour,
        pattern = boundedGrid(
          lowBound = Vector(-1.0, 0.0, 1.0),
          highBound = Vector(1.0, 0.5, 128.0),
          definition = Vector(10, 5, 128)
        ),
        continuous = Vector(C(0.0, 1.0), C(0.0, 1.0), C(0.0, math.Pi), C(0.0, math.Pi), C(0.0, math.Pi)),
        dilation = 1.0,
        maxRareSample = 10)

      def evolution =
        ppse.
          until(afterGeneration(100)).
          trace: (s, is) =>
            scribe.info(s"Generation ${s.generation}")
            val result = EMPPSE2.result(ppse, is, s)
            println("patterns " + result.length)


      val rng = newRNG(42)
      val (finalState, finalPopulation) = evolution.eval(rng)

      val result = EMPPSE2.result(ppse, finalPopulation, finalState)
      println(result)
//      config.trace.foreach: m =>
//        m.delete(swallowIOExceptions = true)
//        for c <- runInfo do m.appendLine(s"${c.evaluation}, ${c.converge.error}, ${c.converge.missed}")




    case _ =>



