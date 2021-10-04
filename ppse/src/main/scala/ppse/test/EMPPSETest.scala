package ppse.test

import better.files._
import mgo.evolution._
import mgo.evolution.niche._
import org.locationtech.jts.geom.{Coordinate, GeometryFactory}
import ppse.em.EMPPSE.Individual
import ppse.em._
import scopt._
import squants.mass.Density

import scala.collection.mutable.ListBuffer


object EMPPSETest extends App {


  case class Config(
    map: Option[File] = None,
    trace: Option[File] = None,
    traceGMM: Option[File] = None,
    traceHit: Option[File] = None)

  val builder = OParser.builder[Config]

  val parser = {
    import builder._
    OParser.sequence(
      programName("ppsetest"),
      head("scopt", "4.x"),
      opt[String]('m', "map").action((x, c) => c.copy(map = Some(File(x)))),
      opt[String]('t', "trace").action((x, c) => c.copy(trace = Some(File(x)))),
      opt[String]('g', "trace-gmm").action((x, c) => c.copy(traceGMM = Some(File(x)))),
      opt[String]('h', "trace-hit").action((x, c) => c.copy(traceHit = Some(File(x))))
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      val ppse = EMPPSE(
        lambda = 10,
        phenotype = Benchmark.pow,
        pattern =
          boundedGrid(
            lowBound = Vector(0.0, 0.0),
            highBound = Vector(1.0, 1.0),
            definition = Vector(50, 50)),
        continuous = Vector.fill(2)(C(0.0, 1.0)),
        dilation = 2.0,
        fitOnRarest = 100)

      case class Converge(evaluated: Long, hitMap: algorithm.HitMap, gmm: Option[GMM], individuals: Vector[Individual[Vector[Double]]])
      val converge = ListBuffer[Converge]()

      def evolution =
        ppse.
          until(afterGeneration(1000)).
          trace { (s, is) =>
            val c = Converge(s.evaluated, s.s.hitmap, s.s.gmm.map(_._1), is)
            converge += c
            println(s"Generation ${s.generation}")
          }

      val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

      //println(EMPPSE.result(ppse, finalPopulation).mkString("\n"))
      def result = EMPPSE.result(ppse, finalPopulation, finalState)

      println(s"Delta to uniform ${Benchmark.compareToUniformBenchmark(ppse.pattern, result.map(r => r.pattern -> r.density))}")

      config.map.foreach { m => m.write(result.map { r => r.phenotype.mkString(", ") + s", ${r.density}" }.mkString("\n")) }
      config.trace.foreach { m =>
        m.delete(swallowIOExceptions = true)
        for (c <- converge) m.appendLine(s"${c.evaluated}, ${c.hitMap.size}")
      }

      config.traceGMM.foreach { m =>
        m.delete(swallowIOExceptions = true)
        m.createDirectories()
        for (c <- converge) {
          c.gmm match {
            case Some(gmm) =>
              (m / "weights.csv").appendLine(s"${c.evaluated}, ${gmm.weights.mkString(",")}")
              (m / "means.csv").appendLine(s"${c.evaluated}, ${gmm.means.flatten.mkString(",")}")
              (m / "covariances.csv").appendLine(s"${c.evaluated}, ${gmm.covariances.flatten.flatten.mkString(",")}")

              def hits(i: Individual[Vector[Double]]) = c.hitMap.getOrElse(ppse.pattern(i.phenotype), 1)

              for {
                i <- c.individuals
              } {
                val genome = _root_.mgo.evolution.algorithm.scaleContinuousValues(i.genome._1.toVector, ppse.continuous)
                (m / "points.csv").appendLine(s"${c.evaluated}, ${genome(0)},${genome(1)},${hits(i)}")
              }
            case _ =>
          }
        }
      }

      config.traceHit.foreach { m =>
        import _root_.ppse.tool.Display._
        m.delete(swallowIOExceptions = true)

        for (c <- converge) {
          m.appendLine(s"${c.evaluated}, ${arrayToString(c.hitMap)}")
        }
      }

    case _ =>
  }



}
