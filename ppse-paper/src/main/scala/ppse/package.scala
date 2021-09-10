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

import org.apache.commons.math3.distribution.MixtureMultivariateNormalDistribution

import java.util.Random

package ppse :

  import breeze.linalg.{DenseMatrix, DenseVector, det, sum, `*`}
  import breeze.numerics.log
  import org.apache.commons.math3.linear.CholeskyDecomposition

  type DensityMap = Map[Vector[Int], Double]
  type HitMap = Map[Vector[Int], Int]
  val epsilon = 10-30

  case class GMM(
    means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]],
    weights: Array[Double])

  def gmmToDistribution(gmm: GMM, random: Random): MixtureMultivariateNormalDistribution =
    import org.apache.commons.math3.distribution._
    import org.apache.commons.math3.util._

    import scala.jdk.CollectionConverters._

     def apacheRandom(random: util.Random) = new org.apache.commons.math3.random.RandomGenerator :
       override def setSeed(seed: Int): Unit = ???
       override def setSeed(seed: Array[Int]): Unit = ???
       override def setSeed(seed: Long): Unit = ???

       override def nextBytes(bytes: Array[Byte]): Unit = random.nextBytes(bytes)
       override def nextInt(): Int = random.nextInt()
       override def nextInt(n: Int): Int = random.nextInt(n)
       override def nextLong(): Long = random.nextLong()
       override def nextBoolean(): Boolean = random.nextBoolean()
       override def nextFloat(): Float = random.nextFloat()
       override def nextDouble(): Double = random.nextDouble()
       override def nextGaussian(): Double = random.nextGaussian()


    def dist = (gmm.means zip gmm.covariances).map { case (m, c) =>  new MultivariateNormalDistribution(m, c) }
    def pairs = (dist zip gmm.weights).map { case (d, w) => new Pair(java.lang.Double.valueOf(w), d) }.toList

    new MixtureMultivariateNormalDistribution(apacheRandom(random), pairs.asJava)

  def dilateGMM(gmm: GMM, f: Double): GMM =
    gmm.copy(covariances = gmm.covariances.map(_.map(_.map(_ * f))))

  def renormaliseDensityMap(densityMap: DensityMap) =
    val totalDensity = densityMap.values.sum
    densityMap.map((p, density) => (p, density / totalDensity))

  def arrayToDenseMatrix(rows: Int, cols: Int, array: Array[Array[Double]]): DenseMatrix[Double] =
    // we need to transpose the array first because of breeze column first representation of matrices
    DenseMatrix.create(rows, cols, array.transpose.flatten)

  def arrayToDenseMatrix(array: Array[Array[Double]]) =
    assert(!array.isEmpty)
    DenseMatrix.create(rows = array.length, cols = array.head.length, array.flatten)

  def matrixToArray(m: DenseMatrix[Double]): Array[Array[Double]] =
    Array.tabulate(m.rows,m.cols)((i,j)=>m(i,j))

  def matrixToArray(m: DenseMatrix[Double], w: DenseVector[Int]): Array[Array[Double]] =
    matrixToArray(m).zipWithIndex.flatMap{case (v,i)=>Array.fill(w(i))(v)}

  def covariance(x: DenseMatrix[Double], mean: DenseVector[Double]): DenseMatrix[Double] =
    val q = DenseMatrix.tabulate(x.cols,x.cols)((j,k) => Array.tabulate(x.rows)(i => (x(i,j)-mean(j)) * (x(i,k)-mean(k))).sum)
    q.map(_ / (x.rows - 1).toDouble).toDenseMatrix

  def clusterize(x: DenseMatrix[Double], dataWeights: DenseVector[Double], minClusterSize: Int, random: Random): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) =
    import jsat.*
    import jsat.clustering.*
    import jsat.clustering.kmeans.*
    import jsat.linear.distancemetrics.*
    import jsat.classifiers.*
    import scala.jdk.CollectionConverters.*

    val points = matrixToArray(x)
    def computeCentroid(points: Array[Array[Double]], weights: Array[Double]) =
      def average(x: Array[Double], w: Array[Double]) = (x zip w).map { case (x, w) => x * w }.sum / w.sum
      points.transpose.map { coord => average(coord, weights) }


    def buildSingleCluster(): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) =
      val centroids = computeCentroid(points, dataWeights.toArray)
      val weight = new DenseVector(Array(1.0))
      val clusterCovariance =
        val clusterMatrix = DenseMatrix.tabulate(points.length, x.cols)((i, j) => points(i)(j))
        val centroidVector = new DenseVector(centroids)
        covariance(clusterMatrix, centroidVector)
      (new DenseMatrix[Double](1, x.cols, centroids), Array(clusterCovariance), weight)

    val gmeans = new HDBSCAN
    gmeans.setMinClusterSize(minClusterSize)

    if (points.length < gmeans.getMinPoints) buildSingleCluster()
    else
      val dataSet =
        val dataPoints = (points zip dataWeights.toArray).map { (p, w) =>
          new DataPoint(new jsat.linear.DenseVector(p), w)
        }
        new SimpleDataSet(dataPoints.toList.asJava)


      val clusters = gmeans.cluster(dataSet).asScala.map(_.asScala.toArray).toArray
      if (!clusters.isEmpty)
        val centroids =
          clusters.map { cluster =>
            val points = cluster.map(_.getNumericalValues.arrayCopy())
            val weights = cluster.map(_.getWeight)
            computeCentroid(points, weights)
          }

        val means = arrayToDenseMatrix(clusters.length, x.cols, centroids)

        val totalWeight = clusters.flatten.map(_.getWeight).sum
        val weights = new DenseVector(clusters.map(_.map(_.getWeight).sum / totalWeight))

        val covariances = (clusters zip centroids).map { (cluster, centroid) =>
          val clusterMatrix = DenseMatrix.tabulate(cluster.length, x.cols)((i, j) => cluster(i).getNumericalValues.get(j))
          val centroidVector = new DenseVector(centroid)
          covariance(clusterMatrix, centroidVector)
        }

        (means, covariances, weights)
      else buildSingleCluster()





  /**
   * Full covariance Gaussian Mixture Model, trained using Expectation Maximization.
   *
   * @param x data points
   */
  def initializeAndFitGMM(
    iterations: Int,
    tolerance: Double,
    x: DenseMatrix[Double],
    dataWeights: DenseVector[Double],
    minClusterSize: Int,
    random: Random): GMM =
    val (means, covariances, weights) = clusterize(x, dataWeights, minClusterSize, random)
    val newComponents = means.rows

    fitGMM(
      x = x,
      dataWeights = dataWeights,
      means = means,
      covariances = covariances,
      weights = weights,
      components = newComponents,
      iterations = iterations,
      tolerance = tolerance
    )


  @scala.annotation.tailrec def fitGMM(
    x: DenseMatrix[Double],
    dataWeights: DenseVector[Double],
    means: DenseMatrix[Double],
    covariances: Array[DenseMatrix[Double]],
    weights: DenseVector[Double],
    components: Int,
    iterations: Int,
    tolerance: Double,
    logLikelihood: Double = 0.0): GMM =
    def gmm =
      GMM(
        means = matrixToArray(means),
        covariances = covariances.map(matrixToArray),
        weights = weights.toArray
      )

    iterations match
      case 0 => gmm
      case i =>
        val (updatedLogLikelihood, resp) = eStep(x, dataWeights, means, covariances, weights)
        val (updatedWeights, updatedMeans, updatedCovariances) = mStep(x, dataWeights, resp, components)
        if (math.abs(updatedLogLikelihood - logLikelihood) <= tolerance) gmm
        else fitGMM(
          x = x,
          dataWeights = dataWeights,
          means = updatedMeans,
          covariances = updatedCovariances,
          weights = updatedWeights,
          logLikelihood = updatedLogLikelihood,
          components = components,
          iterations = i - 1,
          tolerance = tolerance)

  /**
   * E-step: compute responsibilities,
   * update resp matrix so that resp[j, k] is the responsibility of cluster k for data point j,
   * to compute likelihood of seeing data point j given cluster k.
   *
   * @param x data points
   * @param means means of the components (clusters)
   * @param covariances covariances of the components (clusters)
   * @param weights weights of the components (clusters)
   */
  def eStep(
    x: DenseMatrix[Double],
    dataWeights: DenseVector[Double],
    means: DenseMatrix[Double],
    covariances: Array[DenseMatrix[Double]],
    weights: DenseVector[Double]): (Double, DenseMatrix[Double]) =
    // for each point and each component
    // the matrix containing the probability of point i for component k multiplied by the weight (coefficient) of component k
    assert(weights.forall(p=> p <= 1.0 && p >= 0), s"weights=${weights}")
    //assert(dataWeights.forall(p=> p >= 1.0), s"dataweights=${dataWeights}")
    //assert(x.rows>10,s"data=$x")
    val resp = compute_log_likelihood(x, dataWeights, means, covariances, weights)
    //assert(resp.forall(p=> p > 0), s"RESP=${resp}")
    // for each point, the sum of all likelihoods for all components
    val resp_sum = sum(resp(*, ::))
    //println(s"resp_sum=$resp_sum")
    val log_likelihood = sum(log(resp_sum))
    // divide the responsibility by the sum for each point
    val updatedResp = DenseMatrix.tabulate(resp.rows, resp.cols)((i,j)=>resp(i,j) / (if (resp_sum(i) == 0) 1.0 else resp_sum(i)))
    assert(updatedResp.forall(p=> p <= 1.0 && p >= 0),s"UPDATED_RESP (${updatedResp.rows},${updatedResp.cols}) =${updatedResp}")
    //    assert(sum(updatedResp(*, ::)).forall(p=> p==1.0),s"sums=${sum(updatedResp(*, ::))}")
    (log_likelihood, updatedResp)



  /**
   * Compute the log likelihood (used for e step).
   * @param x data points
   * @param means means of the components (clusters)
   * @param covariances covariances of the components (clusters)
   * @param weights weights of the components (clusters)
   */
  def compute_log_likelihood(
    x: DenseMatrix[Double],
    dataWeights: DenseVector[Double],
    means: DenseMatrix[Double],
    covariances: Array[DenseMatrix[Double]],
    weights: DenseVector[Double]): DenseMatrix[Double] =
    import org.apache.commons.math3.linear.{Array2DRowRealMatrix, EigenDecomposition}
    import org.apache.commons.math3.util.FastMath

    val epsilon = {
      val nameField = classOf[EigenDecomposition].getDeclaredField("EPSILON");
      nameField.setAccessible(true);
      nameField.get(null).asInstanceOf[Double]
    }

    DenseMatrix.tabulate(x.rows, weights.length) { (i, k) =>
      val weightedCovariances = covariances(k).map(_ / dataWeights(i))
      val determinant = det(weightedCovariances)
      val mMeans = means(k, ::).inner.toArray
      val dimension = mMeans.size
      val covarianceArray =  matrixToArray(weightedCovariances)

      def density(vals: Array[Double]) =
        def covarianceMatrixInverse =
          val covarianceMatrix = new Array2DRowRealMatrix(covarianceArray)
          val covMatDec = new CholeskyDecomposition(covarianceMatrix)
          covMatDec.getSolver.getInverse


        def getExponentTerm(values: Array[Double]) =
          val centered = Array.tabulate(values.length) { i => values(i) - mMeans(i)}
          val preMultiplied = covarianceMatrixInverse.preMultiply(centered)
          val sum = (preMultiplied zip centered).map(_ * _).sum
          FastMath.exp(-0.5 * sum)

        FastMath.pow(2 * FastMath.PI, -0.5 * dimension) * FastMath.pow(determinant, -0.5) * getExponentTerm(vals)


      if(determinant <= epsilon) epsilon
      else density(x(i, ::).inner.toArray) * weights(k)
    }


  /**
   * M-step, update parameters.
   * @param X data points
   */
  def mStep(X: DenseMatrix[Double], dataWeights: DenseVector[Double], resp: DenseMatrix[Double], components: Int): (DenseVector[Double], DenseMatrix[Double], Array[DenseMatrix[Double]]) =
    // sum the columns to get total responsibility assigned to each cluster, N^{soft}
    val resp_t = resp.t
    val component_weights = sum(resp_t(*, ::))
    // normalized weights (mixture coefficients)
    val weights = component_weights.map(_ / X.rows.toDouble)
    // means
    // for all components : the sum of the product of responsibility by point values weighted by point weight
    val weighted_sum = resp_t * DenseMatrix.tabulate(X.rows, X.cols)((i,j)=>X(i,j)*dataWeights(i))
    // for all components : the sum of the product of responsibility by point weight
    val weighted_resp = resp_t * dataWeights.toDenseMatrix.t
    val means = DenseMatrix.tabulate(weighted_sum.rows, weighted_sum.cols)((i,j)=>weighted_sum(i,j)/weighted_resp(i,0))

    val covariances = Array.tabulate(components) { k =>
      val mean = means(k, ::)
      val w_sum = DenseMatrix.tabulate(X.cols,X.cols) {
        (covRow, covCol) => Array.tabulate(X.rows) {
          i => (X(i,covRow) - mean(covRow)) * (X(i,covCol) - mean(covCol)) * resp_t(k,i) * dataWeights(i)
        }.sum
      }
      w_sum.map(_ / component_weights(k))
    }
    (weights, means, covariances)


  /* ------- Rejection sampling ---------- */

  def toRejectionSampler(gmm: GMM, rng: Random) =
    val distribution = gmmToDistribution(gmm, rng)

    def sample() =
      val x = distribution.sample()
      (x, distribution.density(x))

    def accept(p: Array[Double]) = p.forall(_ >= 0.0) && p.forall(_ <= 1.0)
    RejectionSampler(sample, accept)

  case class RejectionSamplerState(test: Long = 0L, pass: Long = 0L):
    def inverseProbability() = test.toDouble / pass

  class RejectionSampler(sampleFunction: () => (Array[Double], Double), accept: Array[Double] => Boolean) :
    def success(state: RejectionSamplerState) = RejectionSamplerState(state.test + 1, state.pass + 1)
    def fail(state: RejectionSamplerState) = RejectionSamplerState(state.test + 1, state.pass)

    def warmup(n: Int, state: RejectionSamplerState = RejectionSamplerState()): RejectionSamplerState =
      if(n > 0) {
        val (x, _) = sampleFunction()
        if (!accept(x)) warmup(n - 1, fail(state))
        else warmup(n - 1, success(state))
      } else state

    def sample(state: RejectionSamplerState = RejectionSamplerState()): (RejectionSamplerState, (Array[Double], Double)) =
      val (x, density) = sampleFunction()
      if (!accept(x)) sample(fail(state))
      else
        val newState = success(state)
        (newState, (x, density / newState.inverseProbability()))

    def sampleVector(n: Int, state: RejectionSamplerState = RejectionSamplerState(), res: List[(Array[Double], Double)] = List()): (RejectionSamplerState, Array[(Array[Double], Double)]) =
      if(n > 0)
        val (newState, newSample) = sample(state)
        sampleVector(n - 1, newState, newSample :: res)
      else (state, res.reverse.toArray)

  /* --------- Evolutionnary algorithm -------- */

  def breeding(
   genomeSize: Int,
   lambda: Int,
   gmm: Option[(GMM, RejectionSamplerState)],
   random: Random): Array[(Array[Double], Double)] =
   gmm match
    case None =>
      def randomGenome(size: Int, random: Random) = Array.fill(size)(random.nextDouble())
      Array.fill(lambda)((randomGenome(genomeSize, random), 1.0))
    case Some((gmm, rejectionState)) =>
      val rejectionSampler = toRejectionSampler(gmm, random)
      val (_, samples) = rejectionSampler.sampleVector(lambda, rejectionState)
      samples.toArray

  def elitism(
    genomes: Array[Array[Double]],
    patterns: Array[Array[Int]],
    offspringGenomes: Array[(Array[Double], Double)],
    offspringPatterns: Array[Array[Int]]): (Array[Array[Double]], Array[Array[Int]]) =

    def keepFirstGenomes(
      genomes: Array[Array[Double]],
      patterns: Array[Array[Int]],
      offspringGenomes: Array[Array[Double]],
      offspringPatterns: Array[Array[Int]]) =
      ((genomes ++ offspringGenomes) zip (patterns ++ offspringPatterns)).distinctBy { (_, pattern) => pattern }.unzip

    keepFirstGenomes(genomes, patterns, offspringGenomes.map(_._1), offspringPatterns)


  def updateGMM(
    gmm: Option[GMM],
    genomes: Array[Array[Double]],
    patterns: Array[Array[Int]],
    newHitMap: HitMap,
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    warmupSampler: Int,
    minClusterSize: Int,
    random: Random) =

    def weights(patterns: Array[Array[Int]]) =
      val w = patterns.map(p => newHitMap.get(p.toVector).getOrElse(1))
      val max = w.max + 1
      w.map(h => 1.0 - math.pow(h.toDouble / max, 2.0))

    gmm match
      case None =>
        val newGMM =
          initializeAndFitGMM(
            iterations = iterations,
            tolerance = tolerance,
            x = arrayToDenseMatrix(genomes),
            dataWeights = DenseVector(weights(patterns): _*),
            minClusterSize = minClusterSize,
            random = random
          )

        val dilatedGMM = dilateGMM(newGMM, dilation)
        val samplerState = toRejectionSampler(dilatedGMM, random).warmup(warmupSampler)

        (dilatedGMM, samplerState)
      case Some(gmm) =>
        val distribution = gmmToDistribution(gmm, random)

        val gmmValue2 =
          initializeAndFitGMM(
            iterations = iterations,
            tolerance = tolerance,
            x = arrayToDenseMatrix(genomes),
            dataWeights = DenseVector(weights(patterns): _*),
            minClusterSize = minClusterSize,
            random = random
          )

        val dilatedGMM = dilateGMM(gmmValue2, dilation)
        val samplerState = toRejectionSampler(dilatedGMM, random).warmup(warmupSampler)

        (dilatedGMM, samplerState)

  def updateState(
    newGenomes: Array[Array[Double]],
    newPatterns: Array[Array[Int]],
    offspringGenomes: Array[(Array[Double], Double)],
    offspringPatterns: Array[Array[Int]],
    densityMap: DensityMap,
    hitMap: HitMap,
    gmm: Option[GMM],
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    warmupSampler: Int,
    minClusterSize: Int,
    random: Random): (HitMap, Option[(GMM, RejectionSamplerState)], DensityMap) =
    def addHits(offspringPatterns: Array[Array[Int]], hitmap: HitMap): HitMap =
      def hits(map: HitMap, c: Vector[Int]) = map.updated(c, map.getOrElse(c, 0) + 1)
      offspringPatterns.foldLeft(hitmap)((m, p) => hits(m, p.toVector))

    val newHitMap = addHits(offspringPatterns, hitMap)

    gmm match
      case None if newGenomes.size < minClusterSize * 2 => (newHitMap, None, densityMap)
      case None =>
        val newGMM =
          updateGMM(
            gmm = None,
            genomes = newGenomes,
            patterns = newPatterns,
            newHitMap = newHitMap,
            iterations = iterations,
            tolerance = tolerance,
            dilation = dilation,
            warmupSampler = warmupSampler,
            minClusterSize = minClusterSize,
            random = random
          )

        (newHitMap, Some(newGMM), densityMap)
      case Some(gmm) =>
        def newGMM =
          updateGMM(
            gmm = Some(gmm),
            genomes = newGenomes,
            patterns = newPatterns,
            newHitMap = newHitMap,
            iterations = iterations,
            tolerance = tolerance,
            dilation = dilation,
            warmupSampler = warmupSampler,
            minClusterSize = minClusterSize,
            random = random
          )

        def offSpringDensities =
          val groupedGenomes = (offspringGenomes zip offspringPatterns).groupMap(_._2)(_._1)
          groupedGenomes.view.mapValues { v => v.map (p => 1 / p._2) }.toSeq

        def probabilityUpdate(p: (Array[Int], Array[Double])) =
          val (pattern, densities) = p
          val newDensity = densityMap.getOrElse(pattern.toVector, 0.0) + densities.sum
          (pattern.toVector, newDensity)

        def newDensityMap = densityMap ++ offSpringDensities.map(probabilityUpdate)

        (newHitMap, Some(newGMM), newDensityMap)


  @main def powExample =

    def pattern(x: Vector[Double], g: Vector[Int]): Vector[Int] =
      def pow(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 4.0))
      pow(x) zip g map { (f, g) => math.floor(f * g).toInt }

    val genomeSize = 10
    val lambda = 100
    val dimension = 2
    val generations = 200
    val dilation = 2.0

    val intervals = Vector.fill(dimension)(50)

    def evolution(
      genomes: Array[Array[Double]] = Array(),
      patterns: Array[Array[Int]] = Array(),
      densityMap: DensityMap = Map(),
      hitMap: HitMap = Map(),
      gmm: Option[(GMM, RejectionSamplerState)] = None,
      random: Random,
      generation: Int = 0): DensityMap =

      println(s"Computing generation $generation")

      if generation >= generations
      then
        renormaliseDensityMap(densityMap)
      else
        val offSpringGenomes = breeding(genomeSize, lambda, gmm, random)
        val offspringPatterns = offSpringGenomes.map((g, _) => pattern(g.toVector, intervals).toArray)

        val (elitedGenomes, elitedPattern) =
          elitism(
            genomes = genomes,
            patterns = patterns,
            offspringGenomes = offSpringGenomes,
            offspringPatterns = offspringPatterns)

        val (elitedHitMap, elitedGMM, elitedDensity) =
          updateState(
            newGenomes = elitedGenomes,
            newPatterns = elitedPattern,
            offspringGenomes = offSpringGenomes,
            offspringPatterns = offspringPatterns,
            densityMap = densityMap,
            hitMap = hitMap,
            gmm = gmm.map(_._1),
            iterations = 1000,
            tolerance = 0.01,
            dilation = dilation,
            warmupSampler = 1000,
            minClusterSize = 10,
            random = random)

        evolution(elitedGenomes, elitedPattern, elitedDensity, elitedHitMap, elitedGMM, random, generation + 1)

    val densityMap = evolution(random = new Random(42))

    println((0 until dimension).map(d => s"d$d").mkString(",") + ",density")

    for
      (pattern, density) <- densityMap
    do
      println(s"${pattern.mkString(",")},$density")