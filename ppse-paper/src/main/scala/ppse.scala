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



package ppse :

  import algebra.Priority.Fallback
  import org.apache.commons.math3.distribution.MixtureMultivariateNormalDistribution
  import breeze.linalg.{DenseMatrix, DenseVector, `*`, det, sum}
  import breeze.numerics.log
  import org.apache.commons.math3.linear.CholeskyDecomposition

  import util.{Failure, Success, Try}
  import java.util.Random

  type LikelihoodRatioMap = Map[Vector[Int], Double]
  type HitMap = Map[Vector[Int], Int]

  case class GMM(
    means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]],
    weights: Array[Double]):
    def components = means.size

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

  def computePDF(likelihoodRatioMap: LikelihoodRatioMap) =
    val totalDensity = likelihoodRatioMap.values.sum
    likelihoodRatioMap.map((p, density) => (p, density / totalDensity))

  def arrayToDenseMatrix(rows: Int, cols: Int, array: Array[Array[Double]]): DenseMatrix[Double] =
    // we need to transpose the array first because of breeze column first representation of matrices
    DenseMatrix.create(rows, cols, array.transpose.flatten)

  def arrayToDenseMatrix(array: Array[Array[Double]]) =
    assert(!array.isEmpty)
    DenseMatrix.create(rows = array.length, cols = array.head.length, array.flatten)

  def arrayToDenseVector(array: Array[Double]) = DenseVector(array: _*)

  def matrixToArray(m: DenseMatrix[Double]): Array[Array[Double]] =
    Array.tabulate(m.rows,m.cols)((i,j)=>m(i,j))

  def matrixToArray(m: DenseMatrix[Double], w: DenseVector[Int]): Array[Array[Double]] =
    matrixToArray(m).zipWithIndex.flatMap{case (v,i)=>Array.fill(w(i))(v)}

  def vectorToArray(m: DenseVector[Double]): Array[Double] = m.toArray

  def covariance(x: DenseMatrix[Double], mean: DenseVector[Double]): DenseMatrix[Double] =
    val q = DenseMatrix.tabulate(x.cols,x.cols)((j,k) => Array.tabulate(x.rows)(i => (x(i,j)-mean(j)) * (x(i,k)-mean(k))).sum)
    q.map(_ / (x.rows - 1).toDouble).toDenseMatrix

  def clusterize(points: Array[Array[Double]],  dataWeights: Array[Double], minClusterSize: Int, random: Random): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) =
    import jsat.*
    import jsat.clustering.*
    import jsat.clustering.kmeans.*
    import jsat.linear.distancemetrics.*
    import jsat.classifiers.*
    import scala.jdk.CollectionConverters.*

    val pointsSize = points.head.size

    //val points = matrixToArray(x)
    def computeCentroid(points: Array[Array[Double]], weights: Array[Double]) =
      def average(x: Array[Double], w: Array[Double]) = (x zip w).map { case (x, w) => x * w }.sum / w.sum
      points.transpose.map { coord => average(coord, weights) }

    def buildSingleCluster(): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) =
      val centroids = computeCentroid(points, dataWeights)
      val weight = Array(1.0)
      val clusterCovariance =
        val clusterMatrix = DenseMatrix.tabulate(points.length, pointsSize)((i, j) => points(i)(j))
        val centroidVector = new DenseVector(centroids)
        matrixToArray(covariance(clusterMatrix, centroidVector))
      (Array(centroids), Array(clusterCovariance), weight)

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

        val totalWeight = clusters.flatten.map(_.getWeight).sum
        val weights = clusters.map(_.map(_.getWeight).sum / totalWeight)

        val covariances = (clusters zip centroids).map { (cluster, centroid) =>
          val clusterMatrix = DenseMatrix.tabulate(cluster.length, pointsSize)((i, j) => cluster(i).getNumericalValues.get(j))
          val centroidVector = arrayToDenseVector(centroid)
          matrixToArray(covariance(clusterMatrix, centroidVector))
        }

        (centroids, covariances, weights)
      else buildSingleCluster()

  @scala.annotation.tailrec def fitGMM(
    x: Array[Array[Double]],
    dataWeights: Array[Double],
    gmm: GMM,
    iterations: Int,
    tolerance: Double,
    logLikelihood: Double = 0.0): Try[GMM] =

    iterations match
      case 0 => Success(gmm)
      case i =>
        eStep(x, dataWeights, gmm.means, gmm.covariances, gmm.weights) match
          case Success((updatedLogLikelihood, resp)) =>
            if (math.abs(updatedLogLikelihood - logLikelihood) <= tolerance) Success(gmm)
            else {
              val updatedGMM = mStep(x, dataWeights, resp, gmm.components)
              fitGMM(
                x = x,
                dataWeights = dataWeights,
                gmm = updatedGMM,
                logLikelihood = updatedLogLikelihood,
                iterations = i - 1,
                tolerance = tolerance)
            }
          case Failure(e) => Failure(e)


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
    x: Array[Array[Double]],
    dataWeights: Array[Double],
    means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]],
    weights: Array[Double]): util.Try[(Double, Array[Array[Double]])] =
    // for each point and each component
    // the matrix containing the probability of point i for component k multiplied by the weight (coefficient) of component k
    assert(weights.forall(p=> p <= 1.0 && p >= 0), s"weights=${weights}")
    //assert(dataWeights.forall(p=> p >= 1.0), s"dataweights=${dataWeights}")
    //assert(x.rows>10,s"data=$x")
    compute_log_likelihood(arrayToDenseMatrix(x), arrayToDenseVector(dataWeights), arrayToDenseMatrix(means), covariances.map(arrayToDenseMatrix), arrayToDenseVector(weights)).map { resp =>
      //assert(resp.forall(p=> p > 0), s"RESP=${resp}")
      // for each point, the sum of all likelihoods for all components
      val resp_sum = sum(resp(*, ::))
      //println(s"resp_sum=$resp_sum")
      val log_likelihood = sum(log(resp_sum))
      // divide the responsibility by the sum for each point
      val updatedResp = DenseMatrix.tabulate(resp.rows, resp.cols)((i, j) => resp(i, j) / (if (resp_sum(i) == 0) 1.0 else resp_sum(i)))
      assert(updatedResp.forall(p => p <= 1.0 && p >= 0), s"UPDATED_RESP (${updatedResp.rows},${updatedResp.cols}) =${updatedResp}")
      //    assert(sum(updatedResp(*, ::)).forall(p=> p==1.0),s"sums=${sum(updatedResp(*, ::))}")
      (log_likelihood, matrixToArray(updatedResp))
    }



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
    weights: DenseVector[Double]): util.Try[DenseMatrix[Double]] =
    import org.apache.commons.math3.linear.{Array2DRowRealMatrix, EigenDecomposition}
    import org.apache.commons.math3.util.FastMath
    import org.apache.commons.math3.linear.NonPositiveDefiniteMatrixException

    Try {
      DenseMatrix.tabulate(x.rows, weights.length) { (i, k) =>
        val weightedCovariances = covariances(k).map(_ / dataWeights(i))
        val determinant = det(weightedCovariances)
        val mMeans = means(k, ::).inner.toArray
        val dimension = mMeans.size
        val covarianceArray = matrixToArray(weightedCovariances)

        def density(vals: Array[Double]) =
          def covarianceMatrixInverse =
            val covarianceMatrix = new Array2DRowRealMatrix(covarianceArray)
            val covMatDec = new CholeskyDecomposition(covarianceMatrix)
            covMatDec.getSolver.getInverse

          def getExponentTerm(values: Array[Double]) =
            val centered = Array.tabulate(values.length) { i => values(i) - mMeans(i) }
            val preMultiplied = covarianceMatrixInverse.preMultiply(centered)
            val sum = (preMultiplied zip centered).map(_ * _).sum
            FastMath.exp(-0.5 * sum)

          FastMath.pow(2 * FastMath.PI, -0.5 * dimension) * FastMath.pow(determinant, -0.5) * getExponentTerm(vals)

        density(x(i, ::).inner.toArray) * weights(k)
      }
    }


  /**
   * M-step, update parameters.
   * @param X data points
   */
  def mStep(x: Array[Array[Double]], dataWeights: Array[Double], resp: Array[Array[Double]], components: Int): GMM =
    val xMatrix = arrayToDenseMatrix(x)
    val respMatrix = arrayToDenseMatrix(resp)

    // sum the columns to get total responsibility assigned to each cluster, N^{soft}
    val resp_t = respMatrix.t
    val component_weights = sum(resp_t(*, ::))
    // normalized weights (mixture coefficients)
    val weights = component_weights.map(_ / xMatrix.rows.toDouble)
    // means
    // for all components : the sum of the product of responsibility by point values weighted by point weight
    val weighted_sum = resp_t * DenseMatrix.tabulate(xMatrix.rows, xMatrix.cols)((i,j)=> xMatrix(i,j) * dataWeights(i))
    // for all components : the sum of the product of responsibility by point weight
    val weighted_resp = resp_t * arrayToDenseVector(dataWeights).toDenseMatrix.t
    val means = DenseMatrix.tabulate(weighted_sum.rows, weighted_sum.cols)((i,j) => weighted_sum(i,j) / weighted_resp(i,0))

    val covariances = Array.tabulate(components) { k =>
      val mean = means(k, ::)
      val w_sum = DenseMatrix.tabulate(xMatrix.cols, xMatrix.cols) {
        (covRow, covCol) => Array.tabulate(xMatrix.rows) {
          i => (xMatrix(i, covRow) - mean(covRow)) * (xMatrix(i, covCol) - mean(covCol)) * resp_t(k,i) * dataWeights(i)
        }.sum
      }
      matrixToArray(w_sum.map(_ / component_weights(k)))
    }

    GMM(weights = vectorToArray(weights), means = matrixToArray(means), covariances = covariances)


  /* ------- Rejection sampling ---------- */

  def buildRejectionSampler(gmm: GMM, rng: Random) =
    val distribution = gmmToDistribution(gmm, rng)

    def sample() =
      val x = distribution.sample()
      (x, distribution.density(x))

    def accept(p: Array[Double]) = p.forall(_ >= 0.0) && p.forall(_ <= 1.0)
    RejectionSampler(sample, accept)

  case class RejectionSamplerState(test: Long = 0L, pass: Long = 0L):
    def inverseProbability() = test.toDouble / pass


  object RejectionSampler:
    def success(state: RejectionSamplerState) = RejectionSamplerState(state.test + 1, state.pass + 1)
    def fail(state: RejectionSamplerState) = RejectionSamplerState(state.test + 1, state.pass)
    def allFailed(state: RejectionSamplerState) = state.pass == 0L

  case class RejectionSampler(sampleFunction: () => (Array[Double], Double), accept: Array[Double] => Boolean)

  def warmupSampler(sampler: RejectionSampler, n: Int, state: RejectionSamplerState = RejectionSamplerState()): RejectionSamplerState =
    if(n > 0) {
      val (x, _) = sampler.sampleFunction()
      if (!sampler.accept(x)) warmupSampler(sampler, n - 1, RejectionSampler.fail(state))
      else warmupSampler(sampler, n - 1, RejectionSampler.success(state))
    } else state

  def sample(sampler: RejectionSampler, state: RejectionSamplerState = RejectionSamplerState()): (RejectionSamplerState, (Array[Double], Double)) =
    val (x, density) = sampler.sampleFunction()
    if (!sampler.accept(x)) sample(sampler, RejectionSampler.fail(state))
    else
      val newState = RejectionSampler.success(state)
      (newState, (x, density / newState.inverseProbability()))

  def sampleArray(sampler: RejectionSampler, n: Int, state: RejectionSamplerState = RejectionSamplerState(), res: List[(Array[Double], Double)] = List()): (RejectionSamplerState, Array[(Array[Double], Double)]) =
    if(n > 0)
      val (newState, newSample) = sample(sampler, state)
      sampleArray(sampler, n - 1, newState, newSample :: res)
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
      val rejectionSampler = buildRejectionSampler(gmm, random)
      val (_, samples) = sampleArray(rejectionSampler, lambda, rejectionState)
      samples

  def elitism(
    genomes: Array[Array[Double]],
    patterns: Array[Array[Int]],
    offspringGenomes: Array[(Array[Double], Double)],
    offspringPatterns: Array[Array[Int]]): (Array[Array[Double]], Array[Array[Int]]) =
    def allGenomes = genomes ++ offspringGenomes.map(_._1)
    def allPatterns = patterns ++ offspringPatterns

    (allGenomes zip allPatterns).distinctBy { (_, pattern) => pattern }.unzip

  def computeGMM(
    genomes: Array[Array[Double]],
    patterns: Array[Array[Int]],
    hitMap: HitMap,
    fitOnRarest: Int,
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    warmupSampling: Int,
    minClusterSize: Int,
    random: Random) =

    def rarestIndividuals = (genomes zip patterns).sortBy { case (_, p) => hitMap.getOrElse(p.toVector, 1) }.take(fitOnRarest)
    def rarestGenomes = rarestIndividuals.map(_._1)

    val genomeWeights =
      rarestIndividuals.map { case (_, p) =>
        val hits = hitMap.getOrElse(p.toVector, 1)
        if(hits < fitOnRarest) fitOnRarest.toDouble - hits else 1.0
      }

    val (means, covariances, clusterWeights) = clusterize(rarestGenomes, genomeWeights, minClusterSize, random)

    def initialGMM = GMM(means = means, covariances = covariances, weights = clusterWeights)

    fitGMM(
      x = rarestGenomes,
      dataWeights = genomeWeights,
      gmm = initialGMM,
      iterations = iterations,
      tolerance = tolerance
    ) match
      case Success(newGMM) =>
        val rejectionSampler = buildRejectionSampler(newGMM, random)
        val samplerState = warmupSampler(rejectionSampler, warmupSampling)

        if RejectionSampler.allFailed(samplerState)
        then None
        else Some((newGMM, samplerState))
      case Failure(_) => None

  def updateState(
    genomes: Array[Array[Double]],
    patterns: Array[Array[Int]],
    offspringGenomes: Array[(Array[Double], Double)],
    offspringPatterns: Array[Array[Int]],
    likelihoodRatioMap: LikelihoodRatioMap,
    hitMap: HitMap,
    fitOnRarest: Int,
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    warmupSampler: Int,
    minClusterSize: Int,
    random: Random): (HitMap, LikelihoodRatioMap, Option[(GMM, RejectionSamplerState)]) =
    def newHitMap =
      def updateHits(m: HitMap, p: Vector[Int]) = m.updatedWith(p.toVector)(v => Some(v.getOrElse(0) + 1))
      offspringPatterns.foldLeft(hitMap)((m, p) => updateHits(m, p.toVector))

    def newLikelihoodRatioMap =
      def offSpringDensities =
        val groupedGenomes = (offspringGenomes zip offspringPatterns).groupMap(_._2)(_._1)
        groupedGenomes.view.mapValues(v => v.map ((_, density) => 1 / density).sum).toSeq

      def updatePatternDensity(map: LikelihoodRatioMap, pattern: Array[Int], densitiy: Double): LikelihoodRatioMap =
        map.updatedWith(pattern.toVector)( v => Some(v.getOrElse(0.0) + densitiy))

      offSpringDensities.foldLeft(likelihoodRatioMap) { case (map, (pattern, density)) => updatePatternDensity(map, pattern, density) }

    if genomes.size < minClusterSize * 2
    then (newHitMap, newLikelihoodRatioMap, None)
    else
      def newGMM =
        computeGMM(
          genomes = genomes,
          patterns = patterns,
          hitMap = newHitMap,
          fitOnRarest = fitOnRarest,
          iterations = iterations,
          tolerance = tolerance,
          dilation = dilation,
          warmupSampling = warmupSampler,
          minClusterSize = minClusterSize,
          random = random
        )

      (newHitMap, newLikelihoodRatioMap, newGMM)

  def evolution(
    genomeSize: Int,
    lambda: Int,
    generations: Int,
    fitOnRarest: Int,
    pattern: Vector[Double] => Vector[Int],
    genomes: Array[Array[Double]] = Array(),
    patterns: Array[Array[Int]] = Array(),
    likelihoodRatioMap: LikelihoodRatioMap = Map(),
    hitMap: HitMap = Map(),
    gmm: Option[(GMM, RejectionSamplerState)] = None,
    random: Random,
    generation: Int = 0): LikelihoodRatioMap =

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
          offspringPatterns = offspringPatterns)

      val (updatedHitMap, updatedlikelihoodRatioMap, updatedGMM) =
        updateState(
          genomes = elitedGenomes,
          patterns = elitedPattern,
          offspringGenomes = offSpringGenomes,
          offspringPatterns = offspringPatterns,
          likelihoodRatioMap = likelihoodRatioMap,
          hitMap = hitMap,
          fitOnRarest = fitOnRarest,
          iterations = 1000,
          tolerance = 0.01,
          dilation = 1.0,
          warmupSampler = 1000,
          minClusterSize = 10,
          random = random)

      evolution(
        genomeSize = genomeSize,
        lambda = lambda,
        generations = generations,
        fitOnRarest = fitOnRarest,
        pattern = pattern,
        elitedGenomes,
        elitedPattern,
        updatedlikelihoodRatioMap,
        updatedHitMap,
        updatedGMM orElse gmm,
        random,
        generation + 1)

  @main def powExample =

    def pattern(x: Vector[Double], g: Vector[Int]): Vector[Int] =
      def pow(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 4.0))
      pow(x) zip g map { (f, g) => math.floor(f * g).toInt }

    val genomeSize = 10
    val lambda = 100
    val dimension = 2
    val generations = 200
    val dilation = 2.0
    val fitOnRarest = 100

    val divisions = Vector.fill(dimension)(50)

    val pdf = evolution(
      genomeSize = genomeSize,
      lambda = lambda,
      generations = generations,
      fitOnRarest = fitOnRarest,
      pattern = pattern(_, divisions),
      random = new Random(42))

    println((0 until dimension).map(d => s"d$d").mkString(",") + ",density")

    for (pattern, density) <- pdf
    do println(s"${pattern.mkString(",")},$density")