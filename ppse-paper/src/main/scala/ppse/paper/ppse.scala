package ppse.paper

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



object ppse :

  import util.{Failure, Success, Try}
  import java.util.Random

  type LikelihoodRatioMap = Map[Vector[Int], Double]
  type HitMap = Map[Vector[Int], Int]

  /* --------- Evolutionnary algorithm -------- */

  def breeding(
   genomeSize: Int,
   lambda: Int,
   gmm: Option[(GMM, rejection.RejectionSamplerState)],
   random: Random): Array[(Array[Double], Double)] =
   gmm match
    case None =>
      def randomGenome(size: Int, random: Random) = Array.fill(size)(random.nextDouble())
      Array.fill(lambda)((randomGenome(genomeSize, random), 1.0))
    case Some((gmm, rejectionState)) =>
      val rejectionSampler = rejection.buildRejectionSampler(gmm, random)
      val (_, samples) = rejection.sampleArray(rejectionSampler, lambda, rejectionState)
      samples

  def elitism(
    genomes: Array[Array[Double]],
    patterns: Array[Array[Int]],
    offspringGenomes: Array[(Array[Double], Double)],
    offspringPatterns: Array[Array[Int]],
    random: Random): (Array[Array[Double]], Array[Array[Int]]) =
    def allGenomes = genomes ++ offspringGenomes.map(_._1)
    def allPatterns = patterns ++ offspringPatterns

    def shuffled = scala.util.Random(random).shuffle(allGenomes zip allPatterns)
    shuffled.distinctBy { (_, pattern) => pattern.toVector }.toArray.unzip

  def computeGMM(
    genomes: Array[Array[Double]],
    patterns: Array[Array[Int]],
    hitMap: HitMap,
    maxRareSample: Int,
    regularisationEpsilon: Double,
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    warmupSampling: Int,
    minClusterSize: Int,
    random: Random) =

    val rareIndividuals =
      (genomes zip patterns).filter: p =>
        val hits = hitMap.getOrElse(p._2.toVector, 0)
        hits <= maxRareSample
      .map(_._1)
    val res =
      if rareIndividuals.size < minClusterSize
      then None
      else
        Some:
          val x = rareIndividuals
          val (clusterMeans, clusterCovariances, clusterWeights) = clustering.build(x, minClusterSize)

          val newGMM = emgmm.fit(
            components = clusterMeans.length,
            iterations = iterations,
            tolerance = tolerance,
            x = x,
            means = clusterMeans,
            covariances = clusterCovariances,
            weights = clusterWeights,
            regularisationEpsilon = regularisationEpsilon)

          val newTGMM = GMM(newGMM._1.means, newGMM._1.covariances, newGMM._1.weights)

          def gmmWithOutliers = emgmm.integrateOutliers(x, newTGMM, regularisationEpsilon)

          val dilatedGMM = GMM.dilate(gmmWithOutliers, dilation)

          val samplerState =
            val sampler = rejection.buildRejectionSampler(dilatedGMM, random)
            rejection.warmupSampler(sampler, warmupSampling)

          (dilatedGMM, samplerState)
    res//.flatMap(_.toOption)

  def updateState(
    genomes: Array[Array[Double]],
    patterns: Array[Array[Int]],
    offspringGenomes: Array[(Array[Double], Double)],
    offspringPatterns: Array[Array[Int]],
    likelihoodRatioMap: LikelihoodRatioMap,
    hitMap: HitMap,
    maxRareSample: Int,
    regularisationEpsilon: Double,
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    warmupSampler: Int,
    minClusterSize: Int,
    random: Random): (HitMap, LikelihoodRatioMap, Option[(GMM, rejection.RejectionSamplerState)]) =
    val newHitMap =
      def updateHits(m: HitMap, p: Vector[Int]) = m.updatedWith(p)(v => Some(v.getOrElse(0) + 1))
      offspringPatterns.foldLeft(hitMap)((m, p) => updateHits(m, p.toVector))

    def newLikelihoodRatioMap =
      def offSpringDensities =
        val groupedGenomes = (offspringGenomes zip offspringPatterns).groupMap(_._2)(_._1)
        groupedGenomes.view.mapValues(v => v.map ((_, density) => 1 / density).sum).toSeq

      def updatePatternDensity(map: LikelihoodRatioMap, pattern: Array[Int], densitiy: Double): LikelihoodRatioMap =
        map.updatedWith(pattern.toVector)( v => Some(v.getOrElse(0.0) + densitiy))

      offSpringDensities.foldLeft(likelihoodRatioMap) { case (map, (pattern, density)) => updatePatternDensity(map, pattern, density) }

    def newGMM =
      computeGMM(
        genomes = genomes,
        patterns = patterns,
        hitMap = newHitMap,
        maxRareSample = maxRareSample,
        regularisationEpsilon = regularisationEpsilon,
        iterations = iterations,
        tolerance = tolerance,
        dilation = dilation,
        warmupSampling = warmupSampler,
        minClusterSize = minClusterSize,
        random = random
      )

    (newHitMap, newLikelihoodRatioMap, newGMM)


  def computePDF(likelihoodRatioMap: LikelihoodRatioMap) =
    val totalDensity = likelihoodRatioMap.values.sum
    likelihoodRatioMap.map((p, density) => (p, density / totalDensity))


  case class StepInfo(generation: Int, likelihoodRatioMap: LikelihoodRatioMap)

  def evolution(
    genomeSize: Int,
    lambda: Int,
    generations: Int,
    maxRareSample: Int,
    regularisationEpsilon: Double,
    pattern: Vector[Double] => Vector[Int],
    genomes: Array[Array[Double]] = Array(),
    patterns: Array[Array[Int]] = Array(),
    likelihoodRatioMap: LikelihoodRatioMap = Map(),
    hitMap: HitMap = Map(),
    gmm: Option[(GMM, rejection.RejectionSamplerState)] = None,
    random: Random,
    generation: Int = 0,
    trace: StepInfo => Unit = identity): LikelihoodRatioMap =

    trace(StepInfo(generation, likelihoodRatioMap))

    if generation >= generations
    then computePDF(likelihoodRatioMap)
    else
      val offSpringGenomes = breeding(genomeSize, lambda, gmm, random)
      val offspringPatterns = offSpringGenomes.map((g, _) => pattern(g.toVector).toArray)

      val (elitedGenomes, elitedPattern) =
        elitism(
          genomes = genomes,
          patterns = patterns,
          offspringGenomes = offSpringGenomes,
          offspringPatterns = offspringPatterns,
          random = random)

      val (updatedHitMap, updatedlikelihoodRatioMap, updatedGMM) =
        updateState(
          genomes = elitedGenomes,
          patterns = elitedPattern,
          offspringGenomes = offSpringGenomes,
          offspringPatterns = offspringPatterns,
          likelihoodRatioMap = likelihoodRatioMap,
          hitMap = hitMap,
          maxRareSample = maxRareSample,
          regularisationEpsilon = regularisationEpsilon,
          iterations = 1000,
          tolerance = 0.0001,
          dilation = 1.0,
          warmupSampler = 10000,
          minClusterSize = 3,
          random = random)

      evolution(
        genomeSize = genomeSize,
        lambda = lambda,
        generations = generations,
        maxRareSample = maxRareSample,
        regularisationEpsilon = regularisationEpsilon,
        pattern = pattern,
        elitedGenomes,
        elitedPattern,
        updatedlikelihoodRatioMap,
        updatedHitMap,
        updatedGMM,
        random,
        generation + 1,
        trace = trace)

