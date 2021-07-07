package ppse.em

import breeze.linalg.{DenseMatrix, DenseVector, norm, sum}
import org.apache.commons.math3.ml.clustering.{Clusterable, KMeansPlusPlusClusterer, MultiKMeansPlusPlusClusterer}
import org.apache.commons.math3.ml.distance.EuclideanDistance
import org.apache.commons.math3.random.JDKRandomGenerator

import scala.annotation.tailrec
import scala.util.Random
import scala.jdk.CollectionConverters._
import mgo.tools.apacheRandom
/**
 * Simplistic implementation of K-Means.
 */
object KMeans {
  def initializeAndFit(components: Int, x: DenseMatrix[Double], maxIterations: Int, random: Random, replications: Int = 100): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) = {
    val clusterer = new MultiKMeansPlusPlusClusterer(new KMeansPlusPlusClusterer[Clusterable](components, maxIterations, new EuclideanDistance(), apacheRandom(random)), replications)
    def toClusterable(d: DenseVector[Double]): Clusterable = () => d.toArray
    val clusters = clusterer.cluster((0 until x.rows).map(r=>toClusterable(x(r, ::).t)).asJavaCollection).asScala
    val means = DenseMatrix.tabulate(components, x.cols)((i,j)=>clusters(i).getCenter.getPoint.apply(j))
    val covariances = clusters.indices.map{i=>
      val points = clusters(i).getPoints.asScala
      cov(DenseMatrix.tabulate(points.size, x.cols)((i,j)=>points(i).getPoint.apply(j)), means(i,::).t)
    }.toArray
    val weights = new DenseVector(clusters.map(_.getPoints.size().toDouble / x.rows).toArray)
    (means, covariances, weights)
    // chose Random means in data points
//    (0 until replications).map { _ =>
//      val choices = random.shuffle((0 until x.rows).toArray[Int]).take(components)
//      val means = DenseMatrix.tabulate(components, x.cols)((i, j) => x(choices(i), j))
//      val res = fit(components = components, x = x, means = means, maxIterations = maxIterations)
//      (res, minimumSumOfSquares(res._1))
//    }.minBy(_._2)._1
  }
//  def minimumSumOfSquares(means: DenseMatrix[Double]): Double = {
//    1.0
//  }
  @tailrec
  def fit(components: Int, x: DenseMatrix[Double], means: DenseMatrix[Double], maxIterations: Int): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) = {
    val norms = DenseMatrix.tabulate(x.rows, components)((i,j)=>norm(x(i,::).t - means(j,::).t))
    val indices = DenseVector.tabulate(x.rows)(i=>norms(i,::).inner.toArray.indexOf(norms(i,::).inner.toArray.min)).toArray.zipWithIndex
    val updatedMeans = DenseMatrix.tabulate(components, x.cols) { (i, j) =>
      val pointsInCluster = indices.filter(_._1 == i)
      pointsInCluster.map { case (_, pi) => x(pi, j) }.sum / pointsInCluster.length
    }
    if ((sum(means - updatedMeans) == 0) || (maxIterations == 0)) (means, Array.tabulate(components){i=>
      val pointsInCluster = indices.filter(_._1 == i)
      val xInCluster = DenseMatrix.tabulate(pointsInCluster.length, x.cols)((i,j)=>x(pointsInCluster(i)._2,j))
      cov(xInCluster,means(i,::).t)
    }, DenseVector.tabulate(components)(i=>indices.count(_._1 == i).toDouble / x.rows)) else fit(components, x, updatedMeans, maxIterations -1)
  }
  def cov(x: DenseMatrix[Double], mean: DenseVector[Double]): DenseMatrix[Double] = {
    val q = DenseMatrix.tabulate(x.cols,x.cols)((j,k)=>Array.tabulate(x.rows)(i=>(x(i,j)-mean(j))*(x(i,k)-mean(k))).sum)
    (q /:/ (x.rows - 1).toDouble).toDenseMatrix
  }
}
