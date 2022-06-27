//package ppse.test
//
///*
// * Copyright (C) 2021 Romain Reuillon
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Affero General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU Affero General Public License for more details.
// *
// * You should have received a copy of the GNU Affero General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//import better.files._
//import mgo.evolution._
//import mgo.evolution.niche._
//import ppse.em.EMPPSE.Individual
//import ppse.em._
//import scopt._
//
//import java.util.Random
//import scala.collection.mutable.ListBuffer
//
//object BenchmarkUniform extends App{
//
//  def pattern = boundedGrid(
//    lowBound = Vector(0.0, 0.0),
//    highBound = Vector(1.0, 1.0),
//    definition = Vector(50, 50))(_)
//
//  val uniformSampling = Benchmark.uniformDensity(Benchmark.inverseFlower() andThen pattern)
//
//  val compare =
//    for {
//      points <- 100 to 1000000 by 10000
//    } yield {
//      val p = SampleUniform.uniform2D(Benchmark.inverseFlower() andThen pattern, points, new Random(100))
//      points -> Benchmark.compareToUniformBenchmark(p.toVector, uniformSampling.toVector)
//    }
//
//  println(compare.map(c => s"${c._1},${c._2}").mkString("\n"))
//
//
//}
