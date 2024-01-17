package flocking

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

import flocking.datatypes.Angle
import flocking.{Behaviour, Environment, Model}

import java.io.File
import scopt.*
import mgo.evolution.*
import mgo.evolution.niche.*
import mgo.evolution.algorithm.*
import ppse.em.*

@main def flockingPSE(args: String*) =

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
      val pse = PSE(
        lambda = 10,
        phenotype = (d, _) => behaviour(d),
        pattern = boundedGrid(
          lowBound = Vector(-1.0, 0.0, 1.0),
          highBound = Vector(1.0, 0.5, 128.0),
          definition = Vector(10, 5, 128)
        ),
        continuous = Vector(C(0.0, 1.0), C(0.0, 1.0), C(0.0, math.Pi), C(0.0, math.Pi), C(0.0, math.Pi))
      )

      def evolution =
        pse.
          until(afterGeneration(100)).
          trace: (s, is) =>
            scribe.info(s"Generation ${s.generation}")
            val result = PSE.result(pse, is)
            println("patterns " + result.length)


      val rng = newRNG(42)
      val (finalState, finalPopulation) = evolution.eval(rng)

      val result = PSE.result(pse, finalPopulation)
      println(result)
    //      config.trace.foreach: m =>
    //        m.delete(swallowIOExceptions = true)
    //        for c <- runInfo do m.appendLine(s"${c.evaluation}, ${c.converge.error}, ${c.converge.missed}")




    case _ =>



