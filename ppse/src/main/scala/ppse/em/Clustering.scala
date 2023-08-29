package ppse.em

import breeze.linalg.{DenseMatrix, DenseVector, norm, sum}
import jsat.SimpleDataSet
import jsat.classifiers.DataPoint
import ppse.tool.Breeze
//import org.apache.commons.math3.ml.clustering.{Clusterable, KMeansPlusPlusClusterer, MultiKMeansPlusPlusClusterer}
//import org.apache.commons.math3.ml.distance.EuclideanDistance
//import org.apache.commons.math3.random.JDKRandomGenerator

import scala.annotation.tailrec
import scala.util.Random
import scala.jdk.CollectionConverters._
import mgo.tools.apacheRandom
import smile.clustering
import smile.clustering._
import scala.jdk.CollectionConverters._
/**
 * Simplistic implementation of K-Means.
 */
object Clustering {

  def cov(x: DenseMatrix[Double], mean: DenseVector[Double]): DenseMatrix[Double] = {
    val q = DenseMatrix.tabulate(x.cols,x.cols)((j,k)=>Array.tabulate(x.rows)(i=>(x(i,j)-mean(j))*(x(i,k)-mean(k))).sum)
    (q /:/ (x.rows - 1).toDouble).toDenseMatrix
  }

  def computeCentroid(points: Array[Array[Double]], weights: Option[Array[Double]]) =
    def average(x: Array[Double], w: Option[Array[Double]]) =
      w match
        case Some(w) => (x zip w).map { case (x, w) => x * w }.sum / w.sum
        case None => x.sum / x.size

    points.transpose.map { coord => average(coord, weights) }

  def build(x: Array[Array[Double]], minPoints: Int, dataWeights: Option[Array[Double]] = None): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) = {
    //val pointSize = x.head.length
    
    def buildSingleCluster(): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) =
      val centroids = computeCentroid(x, dataWeights)
      val weight = Array(1.0)
      val covariance =
        val clusterMatrix = Breeze.arrayToDenseMatrix(x)
        val centroidVector = new DenseVector[Double](centroids)
        Breeze.matrixToArray(cov(clusterMatrix, centroidVector))

      (Array(centroids), Array(covariance), weight)
    
    import jsat.clustering._
    import jsat.clustering.kmeans._
    import jsat.linear.distancemetrics._

    val hdbScan = new HDBSCAN
    /*
     Setting the number of neighbors to consider, acts as a smoothing over the density estimate (minPoints) and
     the minimum number of data points needed to form a cluster (minClusterSize) to the same value.
     */
    hdbScan.setMinPoints(minPoints)
    hdbScan.setMinClusterSize(minPoints)
    
    if x.length <= hdbScan.getMinPoints
    then buildSingleCluster()
    else
      val dataSet =
        val dataPoints =
          dataWeights match
            case Some(dataWeights) =>
              (x zip dataWeights).map: (p, w) =>
                new DataPoint(new jsat.linear.DenseVector(p), w)
            case None =>
              x.map: x =>
                new DataPoint(new jsat.linear.DenseVector(x))

        new SimpleDataSet(dataPoints.toList.asJava)

      val clusters = hdbScan.cluster(dataSet).asScala.map(_.asScala.toArray).toArray

      if !clusters.isEmpty
      then
        val centroids =
          clusters.map: cluster =>
            val points = cluster.map(_.getNumericalValues.arrayCopy())
            val weights = cluster.map(_.getWeight)
            computeCentroid(points, Some(weights))

        val totalWeight = clusters.flatten.map(_.getWeight).sum
        val weights = clusters.map(_.map(_.getWeight).sum / totalWeight)

        val covariances = (clusters zip centroids).map { case (cluster, centroid) =>
          val clusterMatrix = Breeze.arrayToDenseMatrix(cluster.map(c => c.getNumericalValues.arrayCopy()))
          val centroidVector = new DenseVector[Double](centroid)
          cov(clusterMatrix, centroidVector)
        }

        scribe.debug:
          s"""
             |centroids:
             |${centroids.map { p => "POINT(" + p.mkString(" ") + ")" }.mkString("\n")}
             |${covariances.map { p => "COV\n" + p }.mkString("\n")}
             |""".stripMargin

        //assert(covariances.forall(_.forall(!_.isNaN)),s"covariances with nan: ${covariances.mkString("\n")}")

        (centroids, covariances.map(Breeze.matrixToArray), weights)
      else buildSingleCluster()

  }

}
