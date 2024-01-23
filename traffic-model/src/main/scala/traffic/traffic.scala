package traffic

import mgo.evolution.*
import mgo.evolution.niche.boundedGrid
import ppse.em.EMPPSE2
import scopt.OParser

import java.io.File
import scala.concurrent.duration.Duration

/*
 * Copyright (C) 2024 Romain Reuillon
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

def behaviour(p: Vector[Double], seed: Int) =
  import scala.sys.process.*
  val lines = Process(s"docker run traffic traffic ${p.mkString(" ")} $seed").lazyLines(ProcessLogger.apply(_ => ()))
  //val lines = res.split("\n")
  val speed = lines(0).toDouble
  val patience = lines(1).toDouble
  (speed, patience)

def replicated(p: Vector[Double]) =
  import concurrent.*
  import ExecutionContext.Implicits.global
//  val futs = (100 until 120).map(s => Future(behaviour(p, s)))
//  val (speeds, patiences) = Await.result(Future.sequence(futs), Duration.Inf).unzip

  val (speeds, patiences) = (100 until 120).map(s => behaviour(p, s)).unzip
  Vector(speeds.sum / speeds.size, patiences.sum / patiences.size)

@main def trafficPPSE(args: String*) =
  scribe.Logger.root.withMinimumLevel(scribe.Level.Info).replace()

  case class Config(trace: Option[File] = None)

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
        lambda = 100,
        phenotype = replicated,
        pattern = boundedGrid(
          lowBound = Vector(0.0, 0.0),
          highBound = Vector(2.0, 100.0),
          definition = Vector(20, 20)
        ),
        continuous = Vector(C(0.0, 100.0), C(0.0, 1.0), C(0.0, 1.0), C(0.0, 100.0)),
        dilation = 1.0,
        maxRareSample = 10)

      def evolution =
        ppse.
          until(afterGeneration(200)).
          trace: (s, is) =>
            scribe.info(s"Generation ${s.generation}")
            val result = EMPPSE2.result(ppse, is, s)
            println("patterns " + result.length)


      val rng = newRNG(42)
      val (finalState, finalPopulation) = evolution.eval(rng, parallel = true)

      val result = EMPPSE2.result(ppse, finalPopulation, finalState)
      println(result)
    //      config.trace.foreach: m =>
    //        m.delete(swallowIOExceptions = true)
    //        for c <- runInfo do m.appendLine(s"${c.evaluation}, ${c.converge.error}, ${c.converge.missed}")




    case _ =>