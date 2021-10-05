package ppse.test

import breeze.linalg.DenseVector
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, MultiPolygon, Polygon}
import ppse.test.Benchmark.SquareDiagonalTransformation.MathVectors._

object Benchmark {

  def flower(w: Double = 0.01) = {
    val polygon = createFlower(w)
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

  object SquareDiagonalTransformation {

    object MathVectors {

      type MathVector = Seq[Double]

      //Definitions
      def dimension(v: MathVector): Int = v.length

      def replace(v: MathVector, i: Int, c: Double): MathVector = v.zipWithIndex map { case (cv, iv) => if (iv == i) c else cv }
      def insert(v: MathVector, i: Int, c: Double): MathVector = {
        val (left, right) = v.splitAt(i)
        left ++ Seq(c) ++ right
      }
      def remove(v: MathVector, i: Int): Seq[Double] = v.zipWithIndex.filterNot(_._2 == i).map(_._1)

      def norm(v: MathVector, p: Int): Double = math.pow(v.map(math.abs).map(math.pow(_, p)).sum, 1.0/p)
      def scale(v: MathVector, s: Double): MathVector = v.map(_ * s)
      def normalize(v: MathVector, p: Int): MathVector = {
        val vNorm = norm(v, p)
        if(vNorm != 0) scale(v, 1/vNorm) else v
      }
      def toNorm(v: MathVector, p: Int, d: Double): MathVector = scale(normalize(v, p), d)
      def add(v1: MathVector, v2: MathVector): MathVector = v1 zip v2 map { case (c1, c2) => c1 + c2 }
      def add(v: MathVector, c: Double): MathVector = v.map(_ + c)
      def sub(v1: MathVector, v2: MathVector): MathVector = v1 zip v2 map { case (c1, c2) => c1 - c2 }
      def sub(v: MathVector, c: Double): MathVector = v.map(_ - c)
      def mul(v1: MathVector, v2: MathVector): MathVector = v1 zip v2 map { case (c1, c2) => c1 * c2 }
      def dot(v1: MathVector, v2: MathVector): Double = mul(v1, v2).sum
      def angle(v1: MathVector, v2: MathVector): Double = math.acos(dot(v1, v2) / (norm(v1) * norm(v2)))
      def parallelComponent(v1: MathVector, v2: MathVector): MathVector = {
        val u = normalize(v2, 2)
        scale(u, dot(v1, u))
      }
      def orthogonalComponent(v1: MathVector, v2: MathVector): MathVector = sub(v1, parallelComponent(v1, v2))
      //

      //Currying
      def replace(i: Int, c: Double)(v: MathVector): MathVector = replace(v, i, c)
      def insert(i: Int, c: Double)(v: MathVector): MathVector = insert(v, i, c)
      def remove(i: Int)(v: MathVector): MathVector = remove(v, i)
      def norm(p: Int)(v: MathVector): Double = norm(v, p)
      def scale(s: Double)(v: MathVector): MathVector = scale(v, s)
      def normalize(p: Int)(v: MathVector): MathVector = normalize(v, p)
      def toNorm(p: Int, d: Double)(v: MathVector): MathVector = toNorm(v, p, d)
      def add(v2: MathVector): MathVector => MathVector = (v1: MathVector) => add(v1, v2)
      def add(c: Double)(v: MathVector): MathVector = add(v, c)
      def sub(v2: MathVector): MathVector => MathVector = (v1: MathVector) => sub(v1, v2)
      def sub(c: Double)(v: MathVector): MathVector = sub(v, c)
      def mul(v2: MathVector): MathVector => MathVector = (v1: MathVector) => mul(v1, v2)
      def dot(v2: MathVector): MathVector => Double = (v1: MathVector) => dot(v1, v2)
      def angle(v2: MathVector): MathVector => Double = (v1: MathVector) => angle(v1, v2)
      def parallelComponent(v2: MathVector): MathVector => MathVector = (v1: MathVector) => parallelComponent(v1, v2)
      def orthogonalComponent(v2: MathVector): MathVector => MathVector = (v1: MathVector) => orthogonalComponent(v1, v2)
      //

      //Parameter aliases
      // @inline ?
      def norm(v: MathVector): Double = norm(2)(v)
      def normalize(v: MathVector): MathVector = normalize(2)(v)
      def toNorm(d: Double)(v: MathVector): MathVector = toNorm(2, d)(v)
      def negate(v: MathVector): MathVector = scale(-1)(v)
      //

      //Function aliases
      // @inline ?
      def -(v: MathVector): MathVector = negate(v)
      //

      //Implicit class
      implicit class ImplicitVector(v: MathVector) {

        def dimension: Int = MathVectors.dimension(v)

        //Currying copy
        def replace(i: Int, c: Double): MathVector = MathVectors.replace(i, c)(v)
        def insert(i: Int, c: Double): MathVector = MathVectors.insert(i, c)(v)
        def remove(i: Int): MathVector = MathVectors.remove(i)(v)
        def norm(p: Int): Double = MathVectors.norm(p)(v)
        def scale(s: Double): MathVector = MathVectors.scale(s)(v)
        def normalize(p: Int): MathVector = MathVectors.normalize(p)(v)
        def toNorm(p: Int, d: Double): MathVector = MathVectors.toNorm(p, d)(v)
        def add(ov: MathVector): MathVector = MathVectors.add(ov)(v)
        def add(c: Double): MathVector = MathVectors.add(c)(v)
        def sub(ov: MathVector): MathVector = MathVectors.sub(ov)(v)
        def sub(c: Double): MathVector = MathVectors.sub(c)(v)
        def mul(ov: MathVector): MathVector = MathVectors.mul(ov)(v)
        def dot(ov: MathVector): Double = MathVectors.dot(ov)(v)
        def angle(ov: MathVector): Double = MathVectors.angle(ov)(v)
        def parallelComponent(ov: MathVector): MathVector = MathVectors.parallelComponent(ov)(v)
        def orthogonalComponent(ov: MathVector): MathVector = MathVectors.orthogonalComponent(ov)(v)
        //

        //Parameter aliases copy
        def norm: Double = MathVectors.norm(v)
        def normalize: MathVector = MathVectors.normalize(v)
        def toNorm(d: Double): MathVector = MathVectors.toNorm(d)(v)
        def negate: MathVector = MathVectors.negate(v)
        //

        //Function aliases
        def *(s: Double): MathVector = scale(s)
        def *:(s: Double): MathVector = scale(s)
        def +(ov: MathVector): MathVector = add(ov)
        def -(ov: MathVector): MathVector = sub(ov)
        def ^(ov: MathVector): Double = angle(ov)
        //

        def vectorToString: String = "(" + v.mkString(", ") + ")"
      }
      //

    }

    def proportionBetween(v1: MathVector, v2: MathVector, v: MathVector): Double = {
      val v1v2 = v2.sub(v1)
      val v1v = v.sub(v1)
      val u = v1v2.normalize
      (v1v dot u) / v1v2.norm
    }

    def move(v1: MathVector, v2: MathVector, v: MathVector, f: Double => Double): MathVector = {
      val newNorm = f(proportionBetween(v1, v2, v)) * v2.sub(v1).norm
      v1 + v.sub(v1).toNorm(newNorm)
    }

    def t1Bounds(v: MathVector): (MathVector, MathVector) = {
      val s = v.sum
      if(s < 1) {
        (Seq(0, s), Seq(s, 0))
      } else {
        (Seq(s - 1, 1), Seq(1, s - 1))
      }

    }

    def t2Bounds(v: MathVector): (MathVector, MathVector) = {
      if(v(0) > v(1)) {
        val s = (1 - v(0)) + v(1)
        (Seq(1 - s, 0), Seq(1, s))
      } else {
        val s = v(0) + (1 - v(1))
        (Seq(0, 1 - s), Seq(s, 1))
      }
    }

    /*
    def asinP(p: Double): Double = {
      (asin((p - 0.5)*2)/(Pi/2) + 1)/2
    }

    def asinPN(p: Double, n: Int): Double = {
      var newP = p
      for(_ <- 1 to n) {
        newP = asinP(newP)
      }
      newP
    }
    */

    // 0 is on the diagonal and 1 is on the border
    def t1ff(p: Double): Double = {
      math.pow(p, 2)//TODO
    }

    def t1f(p: Double): Double = {
      var u = (p - 0.5) * 2
      if(u > 0) {
        u = t1ff(u)
      } else {
        u = -t1ff(-u)
      }
      (u + 1)/2
    }

    // 0 is on the border including (0, 0) and 1 is on the border including (1, 1)
    def t2f(p: Double): Double = {
      math.pow(p, 2)//TODO
    }

    // transformation
    def move(v: MathVector): MathVector = {
      val t1B = t1Bounds(v)
      val vt1 = move(t1B._1, t1B._2, v, t1f)
      val t2B = t2Bounds(vt1)
      val vt1t2 = move(t2B._1, t2B._2, vt1, t2f)
      vt1t2
    }

    def vectors(n: Int): Seq[MathVector] = {
      val range = (0 to n).map(_.toDouble / n)
      range.flatMap(x => {
        range.map(y => {
          move(Seq(x, y))
        })
      })
    }

  }
  def createFlower(w:Double = 0.01) = {
    val fact = new GeometryFactory()
    fact.createMultiPolygon(Array(fact.createPolygon(Array(
      new Coordinate(0.5,0.5+w),
      new Coordinate(0.5-w,0.75),
      new Coordinate(0.5,1.0),
      new Coordinate(0.5+w,0.75),
      new Coordinate(0.5,0.5+w)
    )),fact.createPolygon(Array(
      new Coordinate(0.5+w,0.5),
      new Coordinate(0.75,0.5+w),
      new Coordinate(1.0,0.5),
      new Coordinate(0.75,0.5-w),
      new Coordinate(0.5+w,0.5)
    )),fact.createPolygon(Array(
      new Coordinate(0.5,0.5-w),
      new Coordinate(0.5+w,0.25),
      new Coordinate(0.5,0.0),
      new Coordinate(0.5-w,0.25),
      new Coordinate(0.5,0.5-w)
    )),fact.createPolygon(Array(
      new Coordinate(0.5-w,0.5),
      new Coordinate(0.25,0.5-w),
      new Coordinate(0.0,0.5),
      new Coordinate(0.25,0.5+w),
      new Coordinate(0.5-w,0.5)
    ))
    ))
  }

  def inverseFlower(w:Double = 0.01) = {
    val polygon = createFlower(w)
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
  def pow(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 4.0))

  def bisect(wB: Double, wO: Double)(x: Double, y: Double) = {
    def distanceToBisect(x: Double, y: Double) = math.abs(y - x) / math.sqrt(2.0)
    def distanceToOrigin(x: Double, y: Double) = math.hypot(x, y)
    (wB * distanceToBisect(x, y)) * (wO / distanceToOrigin(x, y))
  }

  def pattern(x: Vector[Double], g: Vector[Int]): Vector[Int] =
    x zip g map {
      case (f, g) => math.floor(f * g).toInt
    }

  def uniformDensity(pattern: Vector[Double] => Vector[Int], aLot: Int = 1000000) = SampleUniform.uniform2D(pattern, aLot)

  def compareToUniformBenchmark(density: Vector[(Vector[Int], Double)], uniformDensity: Map[Vector[Int], Double]) = {
    val densityMap = density.toMap

    val deltas =
      for {
        (pp, dp) <- uniformDensity
        du = densityMap.getOrElse(pp, 0.0)
      } yield math.abs(dp - du)

    val decile = deltas.size / 10
    deltas.toSeq.sorted.apply(decile)
  }


}
