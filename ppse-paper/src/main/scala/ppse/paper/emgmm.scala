package ppse.paper

import jsat.SimpleDataSet
import jsat.classifiers.DataPoint
import jsat.clustering.VBGMM
import jsat.linear.DenseVector
import org.apache.commons.math3.distribution.{MixtureMultivariateNormalDistribution, MultivariateNormalDistribution}
import org.apache.commons.math3.stat.correlation.Covariance

import scala.annotation.tailrec
import scala.util.Random
import scala.util.{Failure, Success, Try}
import smile.stat.distribution.MultivariateGaussianMixture

import scala.jdk.CollectionConverters.*

/**
 * EM-GMM implementation.
 * Inspired by the work of Maël Fabien: https://github.com/maelfabien/EM_GMM_HMM
 */
object emgmm:
  /**
   * Full covariance Gaussian Mixture Model, trained using Expectation Maximization.
   *
   * @param x data points
   * @param columns number of data columns
   */
  def initializeAndFit(
    components: Int,
    iterations: Int,
    tolerance: Double,
    regularisationEpsilon: Double,
    x: Array[Array[Double]],
    columns: Int,
    random: Random): (GMM, Seq[Double]) =
    // initialize parameters
    // chose Random means in data points
    val means = random.shuffle(x.indices.toArray[Int]).take(components).map(c => x(c)).toArray
    // set equal weights to all components
    val weights = Array.fill(components)(1.0 / components)
    // compute covariances
    val covariances = Array.fill(components)(covariance(x))

    val (gmm, logLikelihoodTrace) =
      emgmm.fit(
        x = x,
        means = means,
        covariances = covariances,
        weights = weights,
        components = components,
        iterations = iterations,
        tolerance = tolerance,
        regularisationEpsilon = regularisationEpsilon,
        trace = IndexedSeq()
      )

    (gmm, logLikelihoodTrace)

  @tailrec
  def fit(
    x: Array[Array[Double]],
    means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]],
    weights: Array[Double],
    components: Int,
    iterations: Int,
    tolerance: Double,
    regularisationEpsilon: Double,
    logLikelihood: Double = 0.0,
    trace: Seq[Double] = Seq()): (GMM, Seq[Double]) =
    def gmm = GMM(means, covariances, weights)

    iterations match
      case 0 => (gmm, trace)
      case i =>
        val (updatedLogLikelihood, resp) = eStep(x, means, covariances, weights, regularisationEpsilon)
        val (updatedWeights, updatedMeans, updatedCovariances) = mStep(x, resp, components, regularisationEpsilon)
        if (math.abs(updatedLogLikelihood - logLikelihood) <= tolerance) (gmm, trace :+ updatedLogLikelihood)
        else fit(
          x = x,
          means = updatedMeans,
          covariances = updatedCovariances,
          weights = updatedWeights,
          logLikelihood = updatedLogLikelihood,
          regularisationEpsilon = regularisationEpsilon,
          components = components,
          iterations = i - 1,
          tolerance = tolerance,
          trace = trace :+ updatedLogLikelihood)


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
    means: Array[Array[Double]],
    _covariances: Array[Array[Array[Double]]],
    weights: Array[Double],
    regularisationEpsilon: Double): (Double, Array[Array[Double]]) =
    // resp matrix
    val covariances = _covariances.map(c => regularize(c, regularisationEpsilon))
    val resp = compute_log_likelihood(x, means, covariances, weights, regularisationEpsilon)
    assert(resp.flatten.forall(!_.isNaN))

    val sum = resp.map(_.sum)
    val log_likelihood = sum.map(math.log).sum
    assert(!log_likelihood.isNaN)

    val updatedResp =
      resp.zip(sum).map: (v, div) =>
        v.map: x =>
          if div != 0.0 then x / div else 0.0

    (log_likelihood, updatedResp)

  /**
   * Compute the log likelihood (used for e step).
   * @param x data points
   * @param means means of the components (clusters)
   * @param covariances covariances of the components (clusters)
   * @param weights weights of the components (clusters)
   */
  def compute_log_likelihood(x: Array[Array[Double]], means: Array[Array[Double]], covariances: Array[Array[Array[Double]]], weights: Array[Double], regularisationEpsilon: Double): Array[Array[Double]] =
    val res =
      weights.zipWithIndex.map: (prior, k) =>
        val distributionTry = Try(new MultivariateNormalDistribution(means(k), covariances(k)))
        val distribution =
          distributionTry match
            case Success(v) => v
            case Failure(e) =>
              new MultivariateNormalDistribution(means(k), regularize(covariances(k), regularisationEpsilon))

        x.map: x =>
          distribution.density(x) * prior

    res.transpose

  /**
   * Regularize the matrix by adding a certain value to the diagonal.
   * @param matrix input matrix
   * @param v value to add to the diagonal
   * @return a regularized matrix
   */
  def regularize(matrix: Array[Array[Double]], v: Double): Array[Array[Double]] =
    matrix.zipWithIndex.map: (array, i) =>
      array.zipWithIndex.map: (value, j) =>
        if i == j then value + v else value

  def punctualGMM(x: Array[Array[Double]], regularisationEpsilon: Double): GMM =
    if x.isEmpty
    then GMM.empty
    else
      val size = x.head.length
      val cov =
        Array.tabulate(size, size): (i, j) =>
          if i == j then regularisationEpsilon else 0.0

      val components =
        x.map: x =>
          GMM.Component(x, cov, 1)

      GMM(components)


  /**
   * M-step, update parameters.
   * @param X data points
   */
  def mStep(X: Array[Array[Double]], resp: Array[Array[Double]], components: Int, epsilon: Double): (Array[Double], Array[Array[Double]], Array[Array[Array[Double]]]) =
    // sum the columns to get total responsibility assigned to each cluster, N^{soft}
    val resp_weights = Array.tabulate(components)(i => resp.map(_ (i)).sum)
    // normalized weights
    val weights = resp_weights.map(_ / X.length)
    // means
    val weighted_sum = dot(resp.transpose, X)
    val means = weighted_sum.zip(resp_weights).map { case (array, w) => array.map(_ / w) }


// covariance
    val resp_t = resp.transpose
    val covariances = Array.tabulate(components): k =>
      val diff = X.map(x => x.indices.map(i => x(i) - means(k)(i)).toArray).transpose
      val resp_k = resp_t(k)
      val w_sum = dot(diff.map { l => l.zip(resp_k).map {case (a, b) => a * b }}, diff.transpose)
      regularize(w_sum.map(_.map(_ / resp_weights(k))), epsilon)

    assert(resp.flatten.forall(!_.isNaN))
    assert(means.flatten.forall(!_.isNaN))
    assert(resp_weights.forall(!_.isNaN))
    assert(covariances.flatten.flatten.forall(!_.isNaN))


    (weights, means, covariances)

  def integrateOutliers(x: Array[Array[Double]], gmm: GMM, regularisationEpsilon: Double): GMM =
    if GMM.isEmpty(gmm)
    then punctualGMM(x, regularisationEpsilon)
    else
      val (_, resp) = emgmm.eStep(x, gmm.means, gmm.covariances, gmm.weights, regularisationEpsilon)

      val excludedIndex = resp.zipWithIndex.filter((r, _) => r.sum == 0.0).map(_._2).zipWithIndex.toMap
      val excluded = excludedIndex.size

      def newResp =
        for
          (r, i) <- resp.zipWithIndex
        yield
          val excludedWeight =
            excludedIndex.get(i) match
              case None => Vector.fill(excluded)(0.0)
              case Some(index) => Vector.tabulate(excluded)(i => if i == index then 1.0 else 0.0)

          r ++ excludedWeight

      val (w, m, c) = emgmm.mStep(x, newResp, gmm.size + excluded, regularisationEpsilon)
      GMM(m, c, w)

  /**
   * 2d matrix dot product.
   * @param A matrix A
   * @param B matrix B
   */
  def dot(A: Array[Array[Double]], B: Array[Array[Double]]): Array[Array[Double]] =
    Array.tabulate(A.length)(i=>B.indices.map(j=>B(j).map(_*A(i)(j))).transpose.map(_.sum).toArray)

  def covariance(x: Array[Array[Double]]) = new Covariance(x).getCovarianceMatrix.getData

class PPSE_VBGMM extends VBGMM:
  def getGMM(x: Array[Array[Double]]): GMM =
    val dataSet =
      val dataPoints = x.map(v => new DataPoint(new DenseVector(v)))
      new SimpleDataSet(dataPoints.toList.asJava)
    val clusters = cluster(dataSet).asScala.toSeq.map(_.asScala.toArray).zipWithIndex.map((d,index)=>(d,math.exp(this.log_pi(index)))).filter(_._1.length > 1)
    val weightSum = clusters.map(_._2).sum
    GMM(clusters.map((c, weight) =>
      val cc = c.map(_.getNumericalValues.arrayCopy())
      GMM.Component(tool.mean(cc), tool.covariance(cc), weight / weightSum))
    )

object GMM:

  def p(gmm: GMM, x: Array[Double]): Double =
    gmm.components.map(c => c.weight * new MultivariateNormalDistribution(c.mean, c.covariance).density(x)).sum

  private def bic(gmm: GMM, x: Array[Array[Double]]): Double =
    val logLikelihood = x.map(v =>
      val p_ = p(gmm, v)
      if p_ > 0 then math.log(p_) else 0d
    ).sum
    logLikelihood - 0.5 * gmm.components.size * math.log(x.length)

  enum IMPL:
    // EMGMM uses HDBSCAN and EMGMM
    // XEMGMM computes EMGMM for all numbers of clusters in 1 x.length/2 and keeps the best BIC
    // SMILE uses a similar approach
    // VBGMM uses the Variational Bayesian approach
    case EMGMM, XEMGMM, SMILE, VBGMM

  def build(
             x: Array[Array[Double]],
             rng: Random,
             minClusterSize: Int,
             regularisationEpsilon: Double,
             impl: IMPL = IMPL.VBGMM
           ): GMM =
    impl match
      case IMPL.EMGMM =>
        val (clusterMeans, clusterCovariances, clusterWeights) = clustering.build(x, minClusterSize)
        emgmm.fit(
          components = clusterMeans.length,
          iterations = 10,
          tolerance = 0.0001,
          x = x,
          means = clusterMeans,
          covariances = clusterCovariances,
          weights = clusterWeights,
          regularisationEpsilon = regularisationEpsilon)._1
      case IMPL.XEMGMM =>
        val max = (1 until x.length / 2).map(k =>
          val gmm = emgmm.initializeAndFit(
            components = k,
            iterations = 100,
            tolerance = 1e-4,
            regularisationEpsilon = regularisationEpsilon,
            x = x,
            columns = x.head.length,
            random = rng
          )._1
          val bic_ = bic(gmm, x)
          (gmm, bic_)
        ).maxBy(_._2)
        max._1
      case IMPL.SMILE =>
        // MultivariateGaussianMixture will fail if less than 20 points
        if x.length < 20 then
          GMM(Seq(GMM.Component(tool.mean(x), tool.covariance(x), 1d)))
        else
          // TODO add Try catch + fallback method?
          GMM(MultivariateGaussianMixture.fit(x).components.map(c => GMM.Component(c.distribution().mean(), c.distribution().cov().toArray, c.priori())).toSeq)
      case IMPL.VBGMM =>
        PPSE_VBGMM().getGMM(x)

  def apply(
   means: Array[Array[Double]],
   covariances: Array[Array[Array[Double]]],
   weights: Array[Double]): GMM =
    val components =
      (means zip covariances zip weights).map:
        case ((m, c), w) => Component(m, c, w)

    GMM(IArray.unsafeFromArray(components))

  def dilate(gmm: GMM, f: Double): GMM =
    def dilatedComponents = gmm.components.map(c => c.copy(covariance = c.covariance.map(_.map(_ * math.pow(f, 2)))))
  
    gmm.copy(dilatedComponents)
  
  def toDistribution(gmm: GMM, random: Random): MixtureMultivariateNormalDistribution =
    import org.apache.commons.math3.distribution._
    import org.apache.commons.math3.util._
  
    import scala.jdk.CollectionConverters._

    def dist = (gmm.means zip gmm.covariances).map { case (m, c) => new MultivariateNormalDistribution(tool.toApacheRandom(random), m, c) }
    def pairs = (dist zip gmm.weights).map { case (d, w) => new Pair(java.lang.Double.valueOf(w), d) }.toList

    new MixtureMultivariateNormalDistribution(tool.toApacheRandom(random), pairs.asJava)

  case class Component(mean: Array[Double], covariance: Array[Array[Double]], weight: Double)

  def empty: GMM = GMM(Seq.empty)

  extension (gmm: GMM)
    def means: Array[Array[Double]] = gmm.components.map(_.mean).toArray
    def covariances: Array[Array[Array[Double]]] = gmm.components.map(_.covariance).toArray
    def weights: Array[Double] = gmm.components.map(_.weight).toArray
    def size: Int = gmm.components.size
    def isEmpty: Boolean = gmm.components.isEmpty

case class GMM(components: Seq[GMM.Component])

