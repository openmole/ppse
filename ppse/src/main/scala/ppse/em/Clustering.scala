package ppse.em

import jsat.SimpleDataSet
import jsat.classifiers.DataPoint
import ppse.tool.*

import scala.jdk.CollectionConverters.*
/**
 * Simplistic implementation of K-Means.
 */
object Clustering:

  def computeCentroid(points: Array[Array[Double]], weights: Option[Array[Double]]): Array[Double] =
    def average(x: Array[Double], w: Option[Array[Double]]) =
      w match
        case Some(w) => (x zip w).map { case (x, w) => x * w }.sum / w.sum
        case None => x.sum / x.length

    points.transpose.map { coord => average(coord, weights) }

  def build(
    x: Array[Array[Double]],
    minPoints: Int,
    dataWeights: Option[Array[Double]] = None): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) =
    //val pointSize = x.head.length
    
    def buildSingleCluster(): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) =
      val centroids = computeCentroid(x, dataWeights)
      val weight = Array(1.0)
      val covariance = Stat.covariance(x)
//        val clusterMatrix = Breeze.arrayToDenseMatrix(x)
//        val centroidVector = new DenseVector[Double](centroids)
//        Breeze.matrixToArray(cov(clusterMatrix, centroidVector))

      (Array(centroids), Array(covariance), weight)
    
    import jsat.clustering.*

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
                //FIXME ignoring weight
                new DataPoint(new jsat.linear.DenseVector(p))//, w)
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
            // FIXME ignoring weights
            //val weights = cluster.map(_.getWeight)
            computeCentroid(points, None)//Some(weights))

        //val totalWeight = clusters.flatten.map(_.getWeight).sum
        //val weights = clusters.map(_.map(_.getWeight).sum / totalWeight)

        val covariances = clusters.map(c => Stat.covariance(c.map(_.getNumericalValues.arrayCopy())))

        scribe.debug:
          s"""
             |centroids:
             |${centroids.map { p => "POINT(" + p.mkString(" ") + ")" }.mkString("\n")}
             |${covariances.map { p => "COV\n" + p }.mkString("\n")}
             |""".stripMargin

        // FIXME ignoring weights
        (centroids, covariances, Array())//weights)
      else buildSingleCluster()



