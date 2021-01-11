package ppse.em

import breeze.linalg.{DenseMatrix, DenseVector, norm, sum}

import scala.annotation.tailrec
import scala.util.Random

/**
 * Simplistic implementation of K-Means.
 */
object KMeans {
  def initializeAndFit(components: Int, x: DenseMatrix[Double], maxIterations: Int, random: Random): (DenseMatrix[Double], Array[DenseMatrix[Double]], DenseVector[Double]) = {
    // chose Random means in data points
    val choices = random.shuffle((0 until x.rows).toArray[Int]).take(components)
    val means = DenseMatrix.tabulate(components,x.cols)((i,j)=>x(choices(i),j))
    fit(components=components, x=x, means = means, maxIterations = maxIterations)
  }
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
