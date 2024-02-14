package ppse.test

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

import better.files.*
import breeze.stats.DescriptiveStats
import mgo.evolution.*
import mgo.evolution.niche.*
import ppse.em.*
import ppse.tool.{Serialization, Stat}
import scopt.*

import scala.collection.mutable.ListBuffer

@main def benchmarkPPSEHypercubes(args: String*): Unit =
  scribe.Logger.root.withMinimumLevel(scribe.Level.Info).replace()
  val dim = 3
  val hypercubes = Hypercubes(
    Hypercubes.Hypercube(Vector.fill(dim)(0.5), 0.02),
    Hypercubes.Hypercube(Vector.fill(dim)(0.05), 0.1),
    Hypercubes.Hypercube(Vector.fill(dim)(0.95), 0.1),
    Hypercubes.Hypercube(Vector.fill(dim)(0.25), 0.015)
  )
  case class Config(
    map: Option[File] = None,
    trace: Option[File] = None,
    draw: Option[File] = None)

  val builder = OParser.builder[Config]

  val parser = {
    import builder.*
    OParser.sequence(
      programName("ppsetest"),
      head("scopt", "4.x"),
      opt[String]('m', "map").action((x, c) => c.copy(map = Some(File(x)))),
      opt[String]('t', "trace").action((x, c) => c.copy(trace = Some(File(x)))),
      opt[String]('d', "draw").action((x, c) => c.copy(draw = Some(File(x))))
    )
  }
  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      val ppse = EMPPSE2(
        lambda = 100,
        phenotype = identity,
        pattern = Hypercubes.pattern(hypercubes, _),
        continuous = Vector.fill(dim)(C(0.0, 1.0)))
        //explorationProbability = 0.1)
      val allPatterns = Hypercubes.allPatterns(hypercubes)
      object RunInfo:
        case class Converge(error: Double, missed: Int)
        case class Draw(points: Vector[Vector[Double]], gmm: Option[GMM])
      case class RunInfo(evaluation: Long, converge: RunInfo.Converge, draw: Option[RunInfo.Draw])
      val runInfo = ListBuffer[RunInfo]()
      def evolution =
        ppse.
          until(afterGeneration(10000)).
          trace: (s, is) =>
            scribe.info(s"Generation ${s.generation}")
            if s.generation > 0 && s.generation % 100 == 0
            then
              def result = EMPPSE2.result(ppse, is, s)
              def referenceDensity(p: Vector[Int]) = Hypercubes.patternDensity(hypercubes, p)
              val indexPattern =
                val all = allPatterns.toSet
                val map = result.groupMap(_.pattern)(_.density).view.mapValues(_.head).toMap
                all.foreach(v => scribe.info(s"pattern: ${v}"))
                all.filter(v => !map.contains(v)).foreach(v => scribe.info(s"not found: ${v}"))
                map.filter((k, _) => all.contains(k))
              val converge =
                val (p,q) = indexPattern.toSeq.map { (p, d) => (referenceDensity(p), d) }.unzip
                val avgError = Stat.jensenShannonDivergence(p,q)
                val missed = allPatterns.size - indexPattern.size
                RunInfo.Converge(avgError, missed)
              val draw = if config.draw.isDefined then Some(RunInfo.Draw(is.map(_.phenotype), s.s.gmm.map(_._1))) else None
              runInfo += RunInfo(s.evaluated, converge, draw)
              scribe.info(s"error ${converge.error} ${converge.missed}")

      val rng = newRNG(42)
      val (finalState, finalPopulation) = evolution.eval(rng)
      val result = EMPPSE2.result(ppse, finalPopulation, finalState)

      config.map.foreach { m => m.write(result.filterNot{ r => r.phenotype.exists(_ > 1.0) || r.phenotype.exists(_ < 0.0)}.map { r => r.phenotype.mkString(", ") + s", ${r.density}" }.mkString("\n")) }
      config.trace.foreach { m =>
        m.delete(swallowIOExceptions = true)
        for c <- runInfo do m.appendLine(s"${c.evaluation}, ${c.converge.error}, ${c.converge.missed}")
      }

      config.draw.foreach { f =>
        def draw = Serialization.PPSEEvolution(runInfo.map(i => Serialization.PPSEDrawState(i.evaluation, i.draw.get.points, i.draw.get.gmm)).toSeq)
        Serialization.save(draw, f.toJava)
      }
    case _ =>
  }


