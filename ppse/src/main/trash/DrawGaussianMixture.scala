//package ppse.test
//
//import org.apache.commons.math3.distribution._
//import org.apache.commons.math3.util._
//import scala.jdk.CollectionConverters._
//import better.files._
//
//object DrawGaussianMixture extends App {
//
//  val gm = mixture()
//  val points = 500000
//  val size = 50
//
//  def pattern(p: Vector[Double]): Vector[Int] = {
//    p.map(x => math.floor(x * size).toInt)
//  }
//
//  val densities =
//    (0 until points).map(_ => gm.sample().toVector).
//      groupBy(pattern).
//      view.mapValues(_.size / points.toDouble).toMap
//
//  File(args(0)).write(densities.map { case (c, d) => c.mkString(", ") + s", $d" }.mkString("\n"))
//
//}
