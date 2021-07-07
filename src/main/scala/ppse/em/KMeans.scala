package ppse.em

import breeze.linalg.{DenseMatrix, DenseVector, norm, sum}
import org.apache.commons.math3.ml.clustering.{Clusterable, KMeansPlusPlusClusterer, MultiKMeansPlusPlusClusterer}
import org.apache.commons.math3.ml.distance.EuclideanDistance
import org.apache.commons.math3.random.JDKRandomGenerator

import scala.annotation.tailrec
import scala.util.Random
import scala.jdk.CollectionConverters._
import mgo.tools.apacheRandom
import smile.clustering._
/**
 * Simplistic implementation of K-Means.
 */
object KMeans {
  def initializeAndFit(components: Int, x: DenseMatrix[Double], dataWeights: DenseVector[Double], maxIterations: Int, random: Random, replications: Int = 100): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) = {
    val points = WDFEMGMM.toArray(x, dataWeights.map(w=>if (w.toInt > 0) w.toInt else 1))
    val clusters = GMeans.fit(points, math.max(components,10))
    val means = WDFEMGMM.toDenseMatrix(clusters.k, x.cols, clusters.centroids)
    val covariances = clusters.centroids.indices.map{i=>
      val pointsIndices = clusters.y.zipWithIndex.filter(_._1 == i).map(_._2)
      cov(DenseMatrix.tabulate(pointsIndices.length, x.cols)((i, j)=>points(pointsIndices(i))(j)),means(i,::).t)
    }.toArray
    println(s"GMeans = ${clusters.k}\n\tsize=${clusters.size.take(clusters.k).mkString(",")}\n\tclusters=\n\t\t${clusters.centroids.map(_.mkString(",")).mkString("\n\t\t")}\n\tcovariances=\n\t\t${covariances.mkString("\n\t\t")}")
    val weights = new DenseVector(clusters.size.take(clusters.k).map(_.toDouble/points.length))
    (means, covariances, weights)
  }
  def cov(x: DenseMatrix[Double], mean: DenseVector[Double]): DenseMatrix[Double] = {
    val q = DenseMatrix.tabulate(x.cols,x.cols)((j,k)=>Array.tabulate(x.rows)(i=>(x(i,j)-mean(j))*(x(i,k)-mean(k))).sum)
    (q /:/ (x.rows - 1).toDouble).toDenseMatrix
  }
}
