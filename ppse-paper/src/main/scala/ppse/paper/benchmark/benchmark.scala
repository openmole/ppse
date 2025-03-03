package ppse.paper.benchmark



/*
 * Copyright (C) 2022 Romain Reuillon
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


import ppse.paper.*

def normalise(p: Seq[Double]) =
  val sum = p.sum
  p.map(_ / sum)

def kullbackLeiblerDivergence(p: Seq[Double], q: Seq[Double], epsilon: Option[Double] = Some(1e-10)): Double =
  val epsilonValue = epsilon.map(e => Math.max(0, e)).getOrElse(0.0)
  (normalise(p) zip normalise(q)).map: (x, y) =>
    val yValue = Math.max(epsilonValue, y)
    if yValue == 0 || x == 0 then 0 else x * math.log(x / yValue)
  .sum

def jeffreysDivergence(p: Seq[Double], q: Seq[Double], epsilon: Option[Double] = Some(1e-10)): Double =
  kullbackLeiblerDivergence(p, q, epsilon) + kullbackLeiblerDivergence(q, p, epsilon)

def kolmogorovSmirnov(p: Seq[Double], q: Seq[Double]) =
  (normalise(p) zip normalise(q)).map((x, y) => math.abs(x-y)).max

def kolmogorovSmirnovTest(p: Seq[Double], q: Seq[Double]): Double =
  import org.apache.commons.math3.stat.inference.*
  val test = new KolmogorovSmirnovTest()
  test.kolmogorovSmirnovTest(normalise(p).toArray, normalise(q).toArray)

/** Jensen-Shannon Divergence */
def jsDivergence(p: Seq[Double], q: Seq[Double], epsilon: Option[Double] = Some(1e-10)): Double =
  val m = p.zip(q).map { case (pi, qi) => (pi + qi) / 2 }
  0.5 *
    kullbackLeiblerDivergence(p, m, epsilon) + 0.5 *
    kullbackLeiblerDivergence(q, m, epsilon)

def totalVariationDistance(p: Seq[Double], q: Seq[Double]): Double =
  require(p.length == q.length, "Distributions must have the same length")
  require(p.forall(_ >= 0) && q.forall(_ >= 0), "Probabilities must be non-negative")
  (normalise(p) zip normalise(q)).map { case (pi, qi) => math.abs(pi - qi) }.sum / 2.0


// Function to compute the Hellinger Distance
def hellingerDistance(p: Seq[Double], q: Seq[Double]): Double =
  require(p.length == q.length, "Distributions must have the same length")
  require(p.forall(_ >= 0) && q.forall(_ >= 0), s"Probabilities must be non-negative $p $q")

  val sumOfSquares =
    (normalise(p) zip normalise(q)).map: (pi, qi) =>
      math.pow(math.sqrt(pi) - math.sqrt(qi), 2)
    .sum

  math.sqrt(sumOfSquares) / math.sqrt(2)

//
//object Hypercubes {
//  case class Hypercube(center: Vector[Double], size: Double):
//    def dimension = center.size
//
//  private def inHypercube(hypercube: Hypercube, point: Vector[Double]) =
//    (point zip hypercube.center).forall { (c, sc) =>
//      c >= sc - hypercube.size / 2 && c < sc + hypercube.size / 2
//    }
//
//  def pattern(ps: Hypercubes, point: Vector[Double]) =
//    ps.patterns.zipWithIndex.find((s, _) => inHypercube(s, point)) match
//      case None => Vector(-1)
//      case Some(_, i) => Vector(i)
//
//  def patternDensity(ps: Hypercubes, p: Vector[Int], excludeFallBack: Boolean = false) =
//    if !excludeFallBack
//    then
//      if isFallbackPattern(p)
//      then patternDensityForRemaining(ps)
//      else patternDensityForHypercube(ps.patterns(p.head))
//    else
//      val totalVolume = ps.patterns.map(volume).sum
//      patternDensityForHypercube(ps.patterns(p.head)) / totalVolume
//
//  def volume(hypercube: Hypercube) = math.pow(hypercube.size, hypercube.dimension)
//
//  def patternDensityForHypercube(hypercube: Hypercube) = volume(hypercube)
//
//  def patternDensityForRemaining(hypercubes: Hypercubes) = 1.0 - hypercubes.patterns.map(volume).sum
//
//  def allPatterns(hypercubes: Hypercubes): Vector[Vector[Int]] =
//    Vector(Vector(-1)) ++ {
//      for
//        (_, i) <- hypercubes.patterns.zipWithIndex
//      yield Vector(i)
//    }
//
//  def isFallbackPattern(p: Vector[Int]) = p.head == -1
//
//  val dim =3
//  val benchmarkPattern = Hypercubes(
//    Seq((Vector(0.1,0.1,0.1),0.1,10),(Vector(0.6,0.7,0.3),0.05,3)).flatMap((v,p,n) =>
//      for
//        x <- 0 to n
//        y <- 0 to n
//        z <- 0 to n
//      yield Hypercubes.Hypercube(Vector(v(0)-p/2+(x*p/n),v(1)-p/2+(y*p/n),v(2)-p/2+(z*p/n)), p/Math.pow(n,dim))):_*
//  )
//}
//
//case class Hypercubes(patterns: Hypercubes.Hypercube*)
//
//@main def hypercubesBenchmark(result: String, replications: Int, generations: Int) =
//  val resultFile = File(result)
//
//  val genomeSize = 3
//  val lambda = 100
//  val maxRareSample = 10
//  val minClusterSize = 3
//  val regularisationEpsilon = 1e-6
//
//  val dim = 3
//  val allPatterns = Hypercubes.allPatterns(Hypercubes.benchmarkPattern)
//  resultFile.delete(true)
//
//  def run(r: Int)(using Async.Spawn) = Future:
//    println(s"Running replication $r")
//
//    def trace(s: ppse.StepInfo) =
//      if s.generation % 10 == 0 && s.generation > 0
//      then
//        val all = allPatterns.toSet.filterNot(Hypercubes.isFallbackPattern)
//        val indexPattern = all.map(k => k -> s.likelihoodRatioMap.getOrElse(k, 0.0)).toMap
//        val missed = all.size - s.likelihoodRatioMap.count((k, _) => all.contains(k))
//
//        val error =
//          val sum = indexPattern.values.sum
//          val normalized =
//            if sum != 0
//            then indexPattern.view.mapValues(_ / sum)
//            else all.map(p => (p, 0.0))
//
//          val (p, q) =
//            normalized.toSeq.map: (p, d) =>
//              (Hypercubes.patternDensity(Hypercubes.benchmarkPattern, p, excludeFallBack = true), d)
//            .unzip
//          kolmogorovSmirnovTest(p, q)
//
//        resultFile.append(s"$r,${s.generation * lambda},$error,$missed\n")
//
//
//    ppse.evolution(
//      genomeSize = genomeSize,
//      lambda = lambda,
//      generations = generations,
//      maxRareSample = maxRareSample,
//      minClusterSize = minClusterSize,
//      regularisationEpsilon = regularisationEpsilon,
//      pattern = Hypercubes.pattern(Hypercubes.benchmarkPattern, _),
//      random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r)),
//      trace = trace)
//
//  Async.blocking:
//    (0 until replications).map(run).awaitAll
//
//@main def hypercubesBenchmarkRandom(result: String, replications: Int, nbPoints: Int) =
//  val resultFile = File(result)
//  val allPatterns = Hypercubes.allPatterns(Hypercubes.benchmarkPattern)
//  resultFile.delete(true)
//
//  def run(r: Int) = Async.blocking:
//    println(s"Running replication $r")
//
//    val random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(r))
//    val resultMap = collection.mutable.HashMap[Vector[Int], Int]()
//
//    for
//      points <- 0 to nbPoints
//    do
//      val (x, y, z) = (random.nextDouble, random.nextDouble, random.nextDouble)
//      val p = Hypercubes.pattern(Hypercubes.benchmarkPattern, Vector(x, y, z))
//
//      resultMap.updateWith(p): hits =>
//        Some(hits.getOrElse(0) + 1)
//
//      if points % 1000 == 0
//      then
//        val all = allPatterns.toSet.filterNot(Hypercubes.isFallbackPattern)
//        val indexPattern = all.map(k => k -> resultMap.getOrElse(k, 0).toDouble / points).toMap
//        val missed = all.size - resultMap.count((k, _) => all.contains(k))
//
//        val error =
//          val (p, q) =
//            indexPattern.toSeq.map: (p, d) =>
//              (Hypercubes.patternDensity(Hypercubes.benchmarkPattern, p, excludeFallBack = true), d)
//            .unzip
//
//          kolmogorovSmirnovTest(p, q)
//
//        resultFile.append(s"$r,$points,$error,$missed\n")
//
//  (0 until replications).foreach(run)


// Modèle SEIR en Scala
