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
import mgo.evolution.*
import mgo.evolution.niche.*
import ppse.em.*
import ppse.tool.{Serialization, Stat}
import scopt.*

import scala.collection.mutable.ListBuffer

//object Benchmark {
//  def compareToUniformBenchmark(allPattern: Vector[Vector[Int]], density: Vector[(Vector[Int], Double)], uniformDensity: Vector[Int] => Double) = {
//    val densityMap = density.toMap
//
//    val deltas =
//      for {
//        (pp, dp) <- uniformDensity
//        du = densityMap.getOrElse(pp, 0.0)
//      } yield math.abs(dp - du)
//
//    //val index = Math.ceil(1 / 2.0 * deltas.size).toInt
//    deltas.sum / deltas.size
//  }
//}



@main def benchmarkPPSE2(args: String*) =
  //val square = PatternSquare(PatternSquare.Square(Vector(0.5, 0.5), 0.1, 100))

  scribe.Logger.root.withMinimumLevel(scribe.Level.Info).replace()

  val square = PatternSquare(
    PatternSquare.Square(Vector(0.5, 0.5), 0.01, 10),
    PatternSquare.Square(Vector(0.25, 0.25), 0.01, 10),
    PatternSquare.Square(Vector(0.25, 0.75), 0.01, 10),
    PatternSquare.Square(Vector(0.75, 0.25), 0.01, 10),
    PatternSquare.Square(Vector(0.75, 0.75), 0.01, 10)
  )

  val genomeSize = 2
  val lambda = 100
  val generations = 200
  val maxRareSample = 10
  val regularisationEpsilon = 1e-6


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
        lambda = lambda,
        phenotype = (_, x) => x,
        pattern = PatternSquare.pattern(square, _),
        continuous = Vector.fill(2)(C(0.0, 1.0)),
        dilation = 1.0,
        maxRareSample = maxRareSample,
        regularisationEpsilon = regularisationEpsilon)

      val allPatterns = PatternSquare.allPatterns2D(square)

      object RunInfo:
        case class Converge(error: Double, missed: Int)
        case class Draw(points: Vector[Vector[Double]], gmm: Option[GMM])



      case class RunInfo(evaluation: Long, converge: RunInfo.Converge, draw: Option[RunInfo.Draw])

      val runInfo = ListBuffer[RunInfo]()

      def evolution =
        ppse.
          until(afterGeneration(generations)).
          trace: (s, is) =>
            //scribe.info(s"Generation ${s.generation}")

            if s.generation > 0 && s.generation % 100 == 0
            then
              def result = EMPPSE2.result(ppse, is, s)
              def referenceDensity(p: Vector[Int]) = PatternSquare.patternDensity(square, p)

              /*val indexPattern =
                val all = allPatterns.toSet
                val map = result.groupMap(_.pattern)(_.density).view.mapValues(_.head).toMap
                map.filter((k, _) => all.contains(k))*/

              val all = allPatterns.toSet
              val map = result.groupMap(_.pattern)(_.density).view.mapValues(_.head).toMap
              val indexPattern = all.map(k => (k, map.getOrElse(k, 0.0))).toMap
              val missed = allPatterns.size - map.count((k, _) => all.contains(k))

              val converge =
//                val avgError =
//                  DescriptiveStats.percentile(indexPattern.removed(Vector(-1, -1, -1)).map { (p, d) => math.abs(referenceDensity(p) - d) }, 0.5)

//                val error =
//                  val sum =
//                    allPatterns.filter(_ == Vector(-1, -1, -1)).map: p =>
//                    //allPatterns.map: p =>
//                      val volume = if p.head == -1 then 1.0 - square.squares.map(PatternSquare.volume(_)).sum else PatternSquare.volume(square.squares(p.head))
//                      val density = indexPattern.getOrElse(p, 0.0)
//                      val reference = referenceDensity(p)
//                      //val error = math.abs((density - reference) / reference)
//                      //assert(error <= 1.0, s"$density $reference $error $p")
//                      val error = if density == 0 || reference==0 then 0 else /*volume **/ (reference*math.log(reference/density) + density*math.log(density/reference))
//                      error
//                    .sum
//                  sum / allPatterns.size
                val error =
                  val (p,q) = indexPattern.toSeq.map { (p, d) => (referenceDensity(p), d) }.unzip
                  Stat.jeffreysDivergence(p,q)
                //val missed = allPatterns.size - indexPattern.size
                RunInfo.Converge(error, missed)

              val draw = if config.draw.isDefined then Some(RunInfo.Draw(is.map(_.phenotype), s.s.gmm.map(_._1))) else None
              runInfo += RunInfo(s.evaluated, converge, draw)

              //
//              println(s.s.hitmap)
              scribe.info(s"${s.generation} ${converge.error} ${converge.missed}")


      val rng = newRNG(42)
      val (finalState, finalPopulation) = evolution.eval(rng)

//      println(converge.map(c => s"${c.generation},${c.delta}").mkString("\n"))
//
//      //println(EMPPSE.result(ppse, finalPopulation).mkString("\n"))
      val result = EMPPSE2.result(ppse, finalPopulation, finalState)
      //println(finalState.s.gmm.map(g => GMM.toString(g._1)))
//
//      val indexPattern = result.groupMap(_.pattern)(_.density).view.mapValues(_.head).toMap
//      val map =
//        (0 until 50) map { x =>
//          (0 until 50) map { y =>
//            indexPattern.getOrElse(Vector(x, y), 0.0)
//          }
//        }
//
//      VisuSquare.displayMap(map)

//      //println(s"Delta to uniform ${Benchmark.compareToUniformBenchmark(ppse.pattern, result.map(r => r.pattern -> r.density))}")

      config.map.foreach: m =>
        m.write(result.filterNot{ r => r.phenotype.exists(_ > 1.0) || r.phenotype.exists(_ < 0.0)}.map { r => r.phenotype.mkString(", ") + s", ${r.density}" }.mkString("\n"))

      config.trace.foreach: m =>
        m.delete(swallowIOExceptions = true)
        for c <- runInfo do m.appendLine(s"${c.evaluation}, ${c.converge.error}, ${c.converge.missed}")


      config.draw.foreach: f =>
        def draw = Serialization.PPSEEvolution(runInfo.map(i => Serialization.PPSEDrawState(i.evaluation, i.draw.get.points, i.draw.get.gmm)).toSeq)
        Serialization.save(draw, f.toJava)
//
//      config.traceGMM.foreach { m =>
//        m.delete(swallowIOExceptions = true)
//        m.createDirectories()
//        for (c <- converge) {
//          c.gmm match {
//            case Some(gmm) =>
//              (m / "weights.csv").appendLine(s"${c.evaluated}, ${gmm.weights.mkString(",")}")
//              (m / "means.csv").appendLine(s"${c.evaluated}, ${gmm.means.flatten.mkString(",")}")
//              (m / "covariances.csv").appendLine(s"${c.evaluated}, ${gmm.covariances.flatten.flatten.mkString(",")}")
//
//              def hits(i: Individual[Vector[Double]]) = c.hitMap.getOrElse(ppse.pattern(i.phenotype), 1)
//
//              for {
//                i <- c.individuals
//              } {
//                val genome = _root_.mgo.evolution.algorithm.scaleContinuousValues(i.genome._1.toVector, ppse.continuous)
//                (m / "points.csv").appendLine(s"${c.evaluated}, ${genome(0)},${genome(1)},${hits(i)}")
//              }
//            case _ =>
//          }
//        }
//      }
//
//      config.traceHit.foreach { m =>
//        import _root_.ppse.tool.Display._
//        m.delete(swallowIOExceptions = true)
//
//        for (c <- converge) {
//          m.appendLine(s"${c.evaluated}, ${arrayToString(c.hitMap)}")
//        }
//      }

    case _ =>
  }


