package ppse.test

import breeze.linalg.DenseVector
import org.locationtech.jts.geom.{Geometry, MultiPolygon, Polygon}

object Benchmark {

  def preparePolygon(inputPolygon: MultiPolygon) = {
    import org.locationtech.jts.geom.GeometryCollection
    import org.locationtech.jts.triangulate.ConformingDelaunayTriangulationBuilder
    val builder = new ConformingDelaunayTriangulationBuilder
    builder.setSites(inputPolygon)
    builder.setConstraints(inputPolygon)
    val triangleCollection = builder.getTriangles(inputPolygon.getFactory).asInstanceOf[GeometryCollection]
    val triangles = (0 until triangleCollection.getNumGeometries).map(i=>triangleCollection.getGeometryN(i)).filter(t=>t.intersection(inputPolygon).getArea() > 0.99 * t.getArea)
    val areas = triangles.map(_.getArea)
    (triangles, areas)
  }
  def sampleInPolygon(triangles: IndexedSeq[Geometry], areas: IndexedSeq[Double])(p: Vector[Double]): Vector[Double] = {
    val totalArea = areas.sum
    val s = p(0) * totalArea
    val t = p(1)
    val index = areas.zipWithIndex.indexWhere(x=>s <= areas.take(x._2).sum + x._1)
    val triangle = triangles(index)
    val previousArea = areas.take(index).sum
    val tmp = Math.sqrt((s - previousArea) / areas(index))
    val a = 1 - tmp
    val b = (1 - t) * tmp
    val c = t * tmp
    val coord = triangle.getCoordinates
    val p1 = coord(0)
    val p2 = coord(1)
    val p3 = coord(2)
    val x1 = p1.x
    val x2 = p2.x
    val x3 = p3.x
    val y1 = p1.y
    val y2 = p2.y
    val y3 = p3.y
    val x = a * x1 + b * x2 + c * x3
    val y = a * y1 + b * y2 + c * y3
    Vector(x,y)
  }

  def pow2(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 2.0))

  def bisect(wB: Double, wO: Double)(x: Double, y: Double) = {
    def distanceToBisect(x: Double, y: Double) = math.abs(y - x) / math.sqrt(2.0)
    def distanceToOrigin(x: Double, y: Double) = math.hypot(x, y)
    (wB * distanceToBisect(x, y)) * (wO / distanceToOrigin(x, y))
  }

  def pattern(x: Vector[Double], g: Vector[Int]): Vector[Int] =
    x zip g map {
      case (f, g) => math.floor(f * g).toInt
    }

  def compareToUniformBenchmark(pattern: Vector[Double] => Vector[Int], density: Vector[(Vector[Int], Double)]) = {
    val aLot = 1000000

    val (uniformDensity, _) = SampleUniform.uniform2D(pattern, aLot)

    val deltas =
      for {
        (pp, dp) <- density
        du = uniformDensity.getOrElse(pp, 0.0)
      } yield math.abs(dp - du)

    deltas.sum
  }


}
