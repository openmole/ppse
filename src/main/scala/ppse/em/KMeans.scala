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
  def initializeAndFit(components: Int, x: DenseMatrix[Double], maxIterations: Int, random: Random, replications: Int = 100): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) = {
    val clusters = GMeans.fit(WDFEMGMM.toArray(x), math.max(components,10))
    val means = WDFEMGMM.toDenseMatrix(clusters.k, x.cols, clusters.centroids)
    val covariances = clusters.centroids.indices.map{i=>
      val pointsIndices = clusters.y.zipWithIndex.filter(_._1 == i).map(_._2)
      cov(DenseMatrix.tabulate(pointsIndices.size, x.cols)((i,j)=>x(pointsIndices(i),j)),means(i,::).t)
    }.toArray
    val weights = new DenseVector(clusters.size.take(clusters.k).map(_.toDouble/x.rows))
//    val clusterer = new MultiKMeansPlusPlusClusterer(new KMeansPlusPlusClusterer[Clusterable](components, maxIterations, new EuclideanDistance(), apacheRandom(random)), replications)
//    def toClusterable(d: DenseVector[Double]): Clusterable = () => d.toArray
//    val clusters = clusterer.cluster((0 until x.rows).map(r=>toClusterable(x(r, ::).t)).asJavaCollection).asScala
//    val means = DenseMatrix.tabulate(components, x.cols)((i,j)=>clusters(i).getCenter.getPoint.apply(j))
//    val covariances = clusters.indices.map{i=>
//      val points = clusters(i).getPoints.asScala
//      cov(DenseMatrix.tabulate(points.size, x.cols)((i,j)=>points(i).getPoint.apply(j)), means(i,::).t)
//    }.toArray
//    val weights = new DenseVector(clusters.map(_.getPoints.size().toDouble / x.rows).toArray)
    (means, covariances, weights)
  }
  def cov(x: DenseMatrix[Double], mean: DenseVector[Double]): DenseMatrix[Double] = {
    val q = DenseMatrix.tabulate(x.cols,x.cols)((j,k)=>Array.tabulate(x.rows)(i=>(x(i,j)-mean(j))*(x(i,k)-mean(k))).sum)
    (q /:/ (x.rows - 1).toDouble).toDenseMatrix
  }
}
