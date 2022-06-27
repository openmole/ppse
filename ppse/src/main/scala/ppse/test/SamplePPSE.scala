package ppse.test

import better.files._
import mgo.evolution._
import mgo.evolution.niche._
import ppse.em.EMPPSE.Individual
import ppse.em._
import scopt._
import scala.collection.mutable.ListBuffer

object SamplePPSE extends App {

  val bench = Benchmark.pow _

  case class Config(
    map: Option[File] = None,
    trace: Option[File] = None,
    traceGMM: Option[File] = None,
    traceHit: Option[File] = None,
    traceDiff: Option[File] = None)

  val builder = OParser.builder[Config]

  val parser = {
    import builder._
    OParser.sequence(
      programName("ppsetest"),
      head("scopt", "4.x"),
      opt[String]('m', "map").action((x, c) => c.copy(map = Some(File(x)))),
      opt[String]('t', "trace").action((x, c) => c.copy(trace = Some(File(x)))),
      opt[String]('g', "trace-gmm").action((x, c) => c.copy(traceGMM = Some(File(x)))),
      opt[String]('h', "trace-hit").action((x, c) => c.copy(traceHit = Some(File(x)))),
      opt[String]('d', "trace-diff").action((x, c) => c.copy(traceDiff = Some(File(x))))
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      val ppse = EMPPSE(
        lambda = 10,
        phenotype = bench,
        pattern =
          boundedGrid(
            lowBound = Vector(0.0, 0.0),
            highBound = Vector(1.0, 1.0),
            definition = Vector(50, 50)),
        continuous = Vector.fill(2)(C(0.0, 1.0)),
        dilation = 1.0,
        fitOnRarest = 100)

      case class Converge(
        evaluated: Long,
        hitMap: algorithm.HitMap,
        gmm: Option[GMM],
        individuals: Vector[Individual[Vector[Double]]],
        state: algorithm.EvolutionState[EMPPSE.EMPPSEState])




      def result(seed: Int) = {
        val converge = ListBuffer[Converge]()

        def evolution =
          ppse.
            until(afterGeneration(1000)).
            trace { (s, is) =>
              val c = Converge(s.evaluated, s.s.hitmap, s.s.gmm.map(_._1), is, s)
              converge += c
              println(s"Generation ${s.generation}")
            }

        val (finalState, finalPopulation) = evolution.eval(new util.Random(seed))
        (EMPPSE.result(ppse, finalPopulation, finalState), converge.toList)
      }

//      def deltaToUniform = Benchmark.compareToUniformBenchmark(result.map(r => r.pattern -> r.density), Benchmark.uniformDensity(bench andThen ppse.pattern).toVector)
//      println(s"Delta to uniform $deltaToUniform")

      config.map.foreach { m => m.write(result(42)._1.map { r => r.phenotype.mkString(", ") + s", ${r.density}" }.mkString("\n")) }

      config.trace.foreach { m =>
        m.delete(swallowIOExceptions = true)
        for (c <- result(42)._2) m.appendLine(s"${c.evaluated}, ${c.hitMap.size}")
      }

      config.traceGMM.foreach { m =>
        m.delete(swallowIOExceptions = true)
        m.createDirectories()
        for (c <- result(42)._2) {
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

        for (c <- result(42)._2) {
          m.appendLine(s"${c.evaluated}, ${arrayToString(c.hitMap)}")
        }
      }

      config.traceDiff.foreach { f =>
        f.delete(swallowIOExceptions = true)

        val unif = Benchmark.powDensityMap(Vector(50, 50))

        val deltas =
          for {
            seed <- 10 until 42
            converge = result(seed)._2
            c <- converge.drop(1)
            if c.state.generation % 10 == 0
          } yield {
            val result = EMPPSE.result(ppse, c.individuals, c.state)
            def deltaToUniform = Benchmark.compareToUniformBenchmark(result.map(r => r.pattern -> r.density), unif.toVector)
            c.state.evaluated -> deltaToUniform
          }

        for {
          (e, d) <- deltas.groupMap(_._1) (_._2)
        } f.appendLine(s"${e},${d.mkString(",")}")
      }

    case _ =>
  }



}
