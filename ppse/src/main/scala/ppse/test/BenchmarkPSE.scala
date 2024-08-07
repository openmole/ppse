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
import org.apache.commons.math3.random.Well44497b
import mgo.evolution.*
import mgo.evolution.algorithm.PSE

import ppse.tool.Serialization
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



@main def benchmarkPSE(args: String*) =
  //val square = PatternSquare(PatternSquare.Square(Vector(0.5, 0.5), 0.1, 100))

  scribe.Logger.root.withMinimumLevel(scribe.Level.Info).replace()

  val square = PatternSquare(
    PatternSquare.Square(Vector(0.5, 0.5), 0.01, 10),
    PatternSquare.Square(Vector(0.25, 0.25), 0.01, 10),
    PatternSquare.Square(Vector(0.25, 0.75), 0.01, 10),
    PatternSquare.Square(Vector(0.75, 0.25), 0.01, 10),
    PatternSquare.Square(Vector(0.75, 0.75), 0.01, 10)
  )

  scribe.info("PSE")

//  println(PatternSquare.pattern(square, Vector(0.549, 0.549)))

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
      opt[String]('t', "trace").action((x, c) => c.copy(trace = Some(File(x))))
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
//      def pattern(x: Vector[Double]) =
//        val p = x.map(_ * 100).map(_.floor.toInt)
//        p.map( c =>
//          if c > 100 then 101
//          else if c < 0 then -1
//          else c
//        )

      val pse = PSE(
        lambda = 100,
        phenotype = (d, _) => d,
        pattern =
          x =>
            println(x -> PatternSquare.pattern(square, x))
            PatternSquare.pattern(square, x)
        ,
        continuous = Vector.fill(2)(C(0.0, 1.0)))

//      def patternToSpace(pattern: Vector[Int]) =
//        def toSpace(x: Int) = x * 1.0 / 50.0
//        pattern.map(p => (toSpace(p), toSpace(p + 1)))

      val allPatterns = PatternSquare.allPatterns2D(square)

      object RunInfo:
        case class Converge(missed: Int)


      case class RunInfo(evaluation: Long, converge: RunInfo.Converge)

      val runInfo = ListBuffer[RunInfo]()

//      val uniformSampling = Benchmark.uniformDensity(Benchmark.squareInSquare _ andThen ppse.pattern)

      def evolution =
        pse.
          until(afterGeneration(5000)).
          trace: (s, is) =>
            scribe.info(s"Generation ${s.generation}")

            if s.generation > 0 && s.generation % 100 == 0
            then
              def result = PSE.result(pse, is)

              def referenceDensity(p: Vector[Int]) =
                if p.head == 1 then 0 else PatternSquare.patternDensity(square, p)
//                val Vector((minX, maxX), (minY, maxY)) = patternToSpace(p)
////                DoublePoisson.inverse(minX, maxX, minY, maxY)
//                0.0

              val indexPattern =
                val all = allPatterns.toSet
                val map = result.map(_.pattern)
                map.filter(all.contains)

              val converge =

                  //              val error =
                  //                allPatterns.map { p =>
                  //                  val density = indexPattern.getOrElse(p, 1.0)
                  //                  ///println(s"density $density ${referenceDensity(p)}")
                  //                  math.abs(density - referenceDensity(p))
                  //                }.sum

                val missed = allPatterns.size - indexPattern.size
                RunInfo.Converge(missed)

              runInfo += RunInfo(s.evaluated, converge)

              //
//              println(s.s.hitmap)
              scribe.info(s"error ${converge.missed}")




      val rng = newRNG(42)
      val (finalState, finalPopulation) = evolution.eval(rng)

//      println(converge.map(c => s"${c.generation},${c.delta}").mkString("\n"))
//
//      //println(EMPPSE.result(ppse, finalPopulation).mkString("\n"))
      val result = PSE.result(pse, finalPopulation)
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

      //config.map.foreach { m => m.write(result.filterNot{ r => r.phenotype.exists(_ > 1.0) || r.phenotype.exists(_ < 0.0)}.map { r => r.phenotype.mkString(", ") + s", ${r.density}" }.mkString("\n")) }

      config.trace.foreach { m =>
        m.delete(swallowIOExceptions = true)
        for c <- runInfo do m.appendLine(s"${c.evaluation}, ${c.converge.missed}")
      }
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


