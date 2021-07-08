package ppse.test

import breeze.linalg.DenseVector
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, MultiPolygon, Polygon}

object Benchmark {

  def flower(w: Double = 0.01) = {
    val fact = new GeometryFactory()
    val polygon = fact.createMultiPolygon(Array(fact.createPolygon(Array(
      new Coordinate(0.5,0.5),
      new Coordinate(0.5-w,0.75),
      new Coordinate(0.5,1.0),
      new Coordinate(0.5+w,0.75),
      new Coordinate(0.5,0.5)
    )),fact.createPolygon(Array(
      new Coordinate(0.5,0.5),
      new Coordinate(0.75,0.5+w),
      new Coordinate(1.0,0.5),
      new Coordinate(0.75,0.5-w),
      new Coordinate(0.5,0.5)
    )),fact.createPolygon(Array(
      new Coordinate(0.5,0.5),
      new Coordinate(0.5+w,0.25),
      new Coordinate(0.5,0.0),
      new Coordinate(0.5-w,0.25),
      new Coordinate(0.5,0.5)
    )),fact.createPolygon(Array(
      new Coordinate(0.5,0.5),
      new Coordinate(0.25,0.5-w),
      new Coordinate(0.0,0.5),
      new Coordinate(0.25,0.5+w),
      new Coordinate(0.5,0.5)
    ))
    ))
    val prep = Benchmark.preparePolygon(polygon)
    Benchmark.sampleInPolygon(prep._1, prep._2) _
  }

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

  def inverseFlower(w:Double = 0.01) = {
    val fact = new GeometryFactory()
    val polygon = fact.createMultiPolygon(Array(fact.createPolygon(Array(
      new Coordinate(0.5,0.5),
      new Coordinate(0.5-w,0.75),
      new Coordinate(0.5,1.0),
      new Coordinate(0.5+w,0.75),
      new Coordinate(0.5,0.5)
    )),fact.createPolygon(Array(
      new Coordinate(0.5,0.5),
      new Coordinate(0.75,0.5+w),
      new Coordinate(1.0,0.5),
      new Coordinate(0.75,0.5-w),
      new Coordinate(0.5,0.5)
    )),fact.createPolygon(Array(
      new Coordinate(0.5,0.5),
      new Coordinate(0.5+w,0.25),
      new Coordinate(0.5,0.0),
      new Coordinate(0.5-w,0.25),
      new Coordinate(0.5,0.5)
    )),fact.createPolygon(Array(
      new Coordinate(0.5,0.5),
      new Coordinate(0.25,0.5-w),
      new Coordinate(0.0,0.5),
      new Coordinate(0.25,0.5+w),
      new Coordinate(0.5,0.5)
    ))
    ))
    val prep = Benchmark.preparePolygon(polygon)
    inverseSampleInPolygon(prep._1, prep._2) _
  }
  def inverseSampleInPolygon(triangles: IndexedSeq[Geometry], areas: IndexedSeq[Double])(p: Vector[Double]): Vector[Double] = {
    def isPointInTriangle(ax: Double, ay: Double, bx: Double, by: Double, cx: Double, cy: Double, px: Double, py: Double) = { // vectors
      val v0x = cx - ax
      val v0y = cy - ay
      val v1x = bx - ax
      val v1y = by - ay
      val v2x = px - ax
      val v2y = py - ay
      // Compute dot products
      val dot00 = v0x * v0x + v0y * v0y
      val dot01 = v0x * v1x + v0y * v1y
      val dot02 = v0x * v2x + v0y * v2y
      val dot11 = v1x * v1x + v1y * v1y
      val dot12 = v1x * v2x + v1y * v2y
      // Compute barycentric coordinates
      val invDenom = 1. / (dot00 * dot11 - dot01 * dot01)
      val u = (dot11 * dot02 - dot01 * dot12) * invDenom
      val v = (dot00 * dot12 - dot01 * dot02) * invDenom
      // Check if point is in triangle
      (u >= 0) && (v >= 0) && (u + v < 1)
    }
    import org.locationtech.jts.geom.Coordinate
    val point = triangles.head.getFactory.createPoint(new Coordinate(p(0),p(1)))
    import scala.jdk.CollectionConverters._
    val triangle = triangles.find(tr=> {
      val a = tr.getCoordinates()(0)
      val b = tr.getCoordinates()(1)
      val c = tr.getCoordinates()(2)
      isPointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, point.getX, point.getY)
    })
    if (triangle.isDefined) {
      import org.locationtech.jts.geom.Coordinate
      val coord = triangle.get.getCoordinates
      val p1 = coord(0)
      val p2 = coord(1)
      val p3 = coord(2)
      val e0 = p2.x - p1.x
      val e1 = p2.y - p1.y
      val f0 = p3.x - p2.x
      val f1 = p3.y - p2.y
      val v2 = (e1 * (p(0) - p1.x) - e0 * (p(1) - p1.y)) / (f0 * e1 - f1 * e0)
      val v1 = (p(1) - p1.y - v2 * f1) / e1
      val r1 = v1 * v1
      val r2 = v2 / v1
      val ind = triangles.indexOf(triangle.get)
      val prev = areas.take(ind).sum
      val d = (r1 * areas(ind) + prev) / areas.sum
      Vector(d, if (r2.isNaN) 0.0 else r2)
    } else Vector(0.0,0.0)

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
