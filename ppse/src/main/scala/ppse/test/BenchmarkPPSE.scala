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

import better.files._
import mgo.evolution._
import mgo.evolution.niche._
import ppse.em.EMPPSE.Individual
import ppse.em._
import scopt._
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


@main def benchmarkPPSE(args: String*) =

  case class Config(
    map: Option[File] = None,
    trace: Option[File] = None)

  val builder = OParser.builder[Config]

  val parser = {
    import builder._
    OParser.sequence(
      programName("ppsetest"),
      head("scopt", "4.x"),
      opt[String]('m', "map").action((x, c) => c.copy(map = Some(File(x)))),
      opt[String]('t', "trace").action((x, c) => c.copy(trace = Some(File(x))))
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      val ppse = EMPPSE(
        lambda = 100,
        phenotype = DoublePoisson.density,
        pattern =
          boundedGrid(
            lowBound = Vector(0.0, 0.0),
            highBound = Vector(1.0, 1.0),
            definition = Vector(50, 50)),
        continuous = Vector.fill(2)(C(0.0, 1.0)),
        dilation = 1.0,
        fitOnRarest = 100)

      def patternToSpace(pattern: Vector[Int]) =
        def toSpace(x: Int) = x * 1.0 / 50.0
        pattern.map(p => (toSpace(p), toSpace(p + 1)))

      val allPatterns =
        for
          x <- 0 until 50
          y <- 0 until 50
        yield Vector(x, y)

      case class Converge(generation: Long, delta: Double)
      val converge = ListBuffer[Converge]()

//      val uniformSampling = Benchmark.uniformDensity(Benchmark.squareInSquare _ andThen ppse.pattern)

      def evolution =
        ppse.
          until(afterGeneration(500)).
          trace { (s, is) =>
            if(s.generation % 10 == 0) {
              def result = EMPPSE.result(ppse, is, s)

              def referenceDensity(p: Vector[Int]) =
                val Vector((minX, maxX), (minY, maxY)) = patternToSpace(p)
                DoublePoisson.inverse(minX, maxX, minY, maxY)

              val indexPattern = result.groupMap(_.pattern)(_.density).view.mapValues(_.head).toMap

              val error =
                allPatterns.map { p =>
                  val density = indexPattern.getOrElse(p, 0.0)
                  math.abs(referenceDensity(p) - density)
                }.sum

              val c = Converge(s.evaluated, error)
              converge += c
            }
            println(s"Generation ${s.generation}")
          }


      val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

//      println(converge.map(c => s"${c.generation},${c.delta}").mkString("\n"))
//
//      //println(EMPPSE.result(ppse, finalPopulation).mkString("\n"))
      val result = EMPPSE.result(ppse, finalPopulation, finalState)
//
//      //println(s"Delta to uniform ${Benchmark.compareToUniformBenchmark(ppse.pattern, result.map(r => r.pattern -> r.density))}")

      config.map.foreach { m => m.write(result.filterNot{ r => r.phenotype.exists(_ > 1.0) || r.phenotype.exists(_ < 0.0)}.map { r => r.phenotype.mkString(", ") + s", ${r.density}" }.mkString("\n")) }
      config.trace.foreach { m =>
        m.delete(swallowIOExceptions = true)
        for (c <- converge) m.appendLine(s"${c.generation}, ${c.delta}")
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


