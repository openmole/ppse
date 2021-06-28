package ppse.test

import mgo._
import ppse.em._
import mgo.evolution._
import niche._
import better.files._

import scopt._

import scala.collection.mutable.ListBuffer

object EMPPSETest extends App {

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
        lambda = 10,
        phenotype = Benchmark.sample,
        pattern =
          boundedGrid(
            lowBound = Vector(0.0, 0.0),
            highBound = Vector(1.0, 1.0),
            definition = Vector(50, 50)),
        continuous = Vector.fill(2)(C(0.0, 1.0)))

      case class Converge(evaluated: Long, discovered: Int)
      val converge = ListBuffer[Converge]()

      def evolution =
        ppse.
          until(afterGeneration(1000)).
          trace { (s, is) =>
            val c = Converge(s.evaluated, s.s.hitmap.size)
            converge += c
            println(s"Generation ${s.generation}")
          }


      val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

      //println(EMPPSE.result(ppse, finalPopulation).mkString("\n"))
      def result = EMPPSE.result(ppse, finalPopulation, finalState)

      config.map.foreach { m => m.write(result.map { r => r.phenotype.mkString(", ") + s", ${r.density}" }.mkString("\n")) }
      config.trace.foreach { m =>
        m.delete(swallowIOExceptions = true)
        for (c <- converge) m.appendLine(s"${c.evaluated}, ${c.discovered}")
      }

    case _ =>
  }



}
