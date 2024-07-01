package traffic

import mgo.evolution.*
import mgo.evolution.niche.boundedGrid
import ppse.em.EMPPSE2
import scopt.OParser

import better.files.*
import scala.concurrent.duration.Duration
import ppse.tool.Serialization
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
  //scribe.info(s"Running docker exec traffic /usr/bin/traffic ${p.mkString(" ")} $seed")
  val lines = Process(s"docker exec traffic /bin/traffic ${p.mkString(" ")} $seed").lazyLines(ProcessLogger.apply(_ => ()))
  //val lines = res.split("\n")
  if lines.size != 2 then Vector(-1.0, -1.0)
  else
    val speed = lines(0).toDouble
    val patience = lines(1).toDouble
    Vector(speed, patience)

//def replicated(p: Vector[Double]) =
//  import concurrent.*
//  import ExecutionContext.Implicits.global
////  val futs = (100 until 120).map(s => Future(behaviour(p, s)))
////  val (speeds, patiences) = Await.result(Future.sequence(futs), Duration.Inf).unzip
//
//  val (speeds, patiences) = (100 until 120).map(s => behaviour(p, s)).unzip
//
//  if speeds.contains(-1.0)
//  then Vector(-1.0, -1.0)
//  else Vector(speeds.sum / speeds.size, patiences.sum / patiences.size)

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
      def maxPatience = 50.0

      val ppse = EMPPSE2(
        lambda = 200,
        phenotype = (rng, x) => behaviour(x ++ Seq(maxPatience), rng.nextInt()),
        pattern = boundedGrid(
          lowBound = Vector(0.0, 0.0),
          highBound = Vector(2.0, 100.0),
          definition = Vector(100, 100)
        ),
        continuous = Vector(C(0.0, 82.0), C(0.0, 0.01), C(0.0, 0.1)),
        dilation = 1.0,
        maxRareSample = 10)

      def saveResultMap(result: Vector[EMPPSE2.Result[Vector[Double]]], file: File) =
        file.parent.toJava.mkdirs()
        file.delete(true)

        file.appendLine("speed,patience,density")

        for
          r <- result
        do file.appendLine(s"${r.pattern(0) / 50.0},${r.pattern(1) * 1.0},${r.density}")


      def evolution =
        ppse.
          until(afterGeneration(1000)).
          trace: (s, is) =>
            scribe.info(s"Generation ${s.generation}")
            val result = EMPPSE2.result(ppse, is, s)
            saveResultMap(result, File(s"/tmp/traffic/traffic-map${s.generation}.csv"))
            saveResultMap(result, File("/tmp/traffic-map.csv"))

      val rng = newRNG(42)
      val (finalState, finalPopulation) = evolution.eval(rng, parallel = true)

      val result = EMPPSE2.result(ppse, finalPopulation, finalState)
      saveResultMap(result, File("/tmp/traffic-map.csv"))

      //println(resultMap(result))


    //      config.trace.foreach: m =>
    //        m.delete(swallowIOExceptions = true)
    //        for c <- runInfo do m.appendLine(s"${c.evaluation}, ${c.converge.error}, ${c.converge.missed}")




    case _ =>