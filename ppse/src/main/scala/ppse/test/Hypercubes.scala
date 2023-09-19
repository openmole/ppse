package ppse.test

object Hypercubes {
  case class Hypercube(center: Vector[Double], size: Double):
    def dimension = center.size

  private def inHypercube(hypercube: Hypercube, point: Vector[Double]) =
    (point zip hypercube.center).forall { (c, sc) =>
      c >= sc - hypercube.size / 2 && c < sc + hypercube.size / 2
    }

  def pattern(ps: Hypercubes, point: Vector[Double]) =
    ps.patterns.zipWithIndex.find((s, _) => inHypercube(s, point)) match
      case None => Vector(-1)
      case Some(_, i) => Vector(i)

  def patternDensity(ps: Hypercubes, p: Vector[Int]) =
    p.head match
      case -1 => patternDensityForRemaining(ps)
      case i => patternDensityForHypercube(ps.patterns(i))

  def volume(hypercube: Hypercube) = math.pow(hypercube.size, hypercube.dimension)

  def patternDensityForHypercube(hypercube: Hypercube) = volume(hypercube)

  def patternDensityForRemaining(hypercubes: Hypercubes) = 1.0 - hypercubes.patterns.map(volume).sum

  def allPatterns(hypercubes: Hypercubes): Vector[Vector[Int]] =
    Vector(Vector(-1)) ++ {
      for
        (_, i) <- hypercubes.patterns.zipWithIndex
      yield Vector(i)
    }
}
case class Hypercubes(patterns: Hypercubes.Hypercube*)
