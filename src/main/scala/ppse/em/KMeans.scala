package ppse.em

import breeze.linalg.{DenseMatrix, DenseVector, norm, sum}
import jsat.SimpleDataSet
import jsat.classifiers.DataPoint
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
object KMeans {
//  def initializeAndFit(components: Int, x: DenseMatrix[Double], dataWeights: DenseVector[Double], maxIterations: Int, random: Random, replications: Int = 100): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) = {
//    val points = WDFEMGMM.toArray(x, dataWeights.map(w=>if (w.toInt > 0) w.toInt else 1))
//    val clusters = GMeans.fit(points, math.max(components,10))
//    val means = WDFEMGMM.toDenseMatrix(clusters.k, x.cols, clusters.centroids)
//    val covariances = clusters.centroids.indices.map{i=>
//      val pointsIndices = clusters.y.zipWithIndex.filter(_._1 == i).map(_._2)
//      cov(DenseMatrix.tabulate(pointsIndices.length, x.cols)((i, j)=>points(pointsIndices(i))(j)),means(i,::).t)
//    }.toArray
//    println(s"GMeans = ${clusters.k}\n\tsize=${clusters.size.take(clusters.k).mkString(",")}\n\tclusters=\n\t\t${clusters.centroids.map(_.mkString(",")).mkString("\n\t\t")}\n\tcovariances=\n\t\t${covariances.mkString("\n\t\t")}")
//    val weights = new DenseVector(clusters.size.take(clusters.k).map(_.toDouble/points.length))
//    (means, covariances, weights)
//  }
  def cov(x: DenseMatrix[Double], mean: DenseVector[Double]): DenseMatrix[Double] = {
    val q = DenseMatrix.tabulate(x.cols,x.cols)((j,k)=>Array.tabulate(x.rows)(i=>(x(i,j)-mean(j))*(x(i,k)-mean(k))).sum)
    (q /:/ (x.rows - 1).toDouble).toDenseMatrix
  }


  def initializeAndFit(x: DenseMatrix[Double], dataWeights: DenseVector[Double], maxIterations: Int, random: Random, replications: Int = 100): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) = {
    val points = WDFEMGMM.toArray(x)
    def computeCentroid(points: Array[Array[Double]], weights: Array[Double]) = {
      def average(x: Array[Double], w: Array[Double]) = (x zip w).map { case (x, w) => x * w }.sum / w.sum
      points.transpose.map { coord => average(coord, weights) }
    }

    def buildSingleCluster(): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) = {
      val centroids = computeCentroid(points, dataWeights.toArray)
      val weight = new DenseVector(Array(1.0))
      val covariance = {
        val clusterMatrix = DenseMatrix.tabulate(points.length, x.cols)((i, j) => points(i)(j))
        val centroidVector = new DenseVector(centroids)
        cov(clusterMatrix, centroidVector)
      }
      (new DenseMatrix[Double](1, x.cols, centroids), Array(covariance), weight)
    }
    import jsat.clustering._
    import jsat.clustering.kmeans._
    import jsat.linear.distancemetrics._

//    val gmeans = new GMeans {
//      rand = random.self
//    }
    val gmeans = new HDBSCAN {
    //  rand = random.self
    }
    if (points.length < gmeans.getMinPoints) {
      buildSingleCluster()
    } else {
      //    gmeans.setMinClusterSize(3)

      val dataSet = {
        val dataPoints = (points zip dataWeights.toArray).map { case (p, w) =>
          new DataPoint(new jsat.linear.DenseVector(p), w)
        }
        assert(points.length == dataWeights.toArray.length)
        new SimpleDataSet(dataPoints.toList.asJava)
      }

      val clusters = gmeans.cluster(dataSet).asScala.map(_.asScala.toArray).toArray

      //    assert(clusters.nonEmpty, s"${clusters.length} Clusters found from ${x.rows} points")

      if (!clusters.isEmpty) {
        val centroids =
          clusters.map { cluster =>
            val points = cluster.map(_.getNumericalValues.arrayCopy())
            val weights = cluster.map(_.getWeight)
            computeCentroid(points, weights)
          }

        val means = WDFEMGMM.toDenseMatrix(clusters.length, x.cols, centroids)

        val totalWeight = clusters.flatten.map(_.getWeight).sum
        val weights = new DenseVector(clusters.map(_.map(_.getWeight).sum / totalWeight))

        val covariances = (clusters zip centroids).map { case (cluster, centroid) =>
          val clusterMatrix = DenseMatrix.tabulate(cluster.length, x.cols)((i, j) => cluster(i).getNumericalValues.get(j))
          val centroidVector = new DenseVector(centroid)
          cov(clusterMatrix, centroidVector)
        }

        (means, covariances, weights)
      } else {
        buildSingleCluster()

      }
    }
  }

}
