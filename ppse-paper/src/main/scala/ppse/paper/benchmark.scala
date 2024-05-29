package ppse.paper


/*
 * Copyright (C) 2022 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

object benchmark:
  def kullbackLeiblerDivergence(p: Seq[Double], q: Seq[Double]): Double =
    p.zip(q).map((x, y) => if y == 0 || x == 0 then 0 else x * math.log(x / y)).sum

  def jeffreysDivergence(p: Seq[Double], q: Seq[Double]): Double =
    kullbackLeiblerDivergence(p, q) + kullbackLeiblerDivergence(q, p)

  object PatternSquare:
    case class Square(center: Vector[Double], size: Double, grid: Int):
      def dimension = center.size

    def inSquare(square: Square, point: Vector[Double]) =
      (point zip square.center).forall: (c, sc) =>
        c >= sc - square.size / 2 && c < sc + square.size / 2

    def patternIntern(square: Square, point: Vector[Double]) =
      def grid(size: Int, x: Vector[Double]) = x.map(_ * size).map(_.floor.toInt)

      val z = 1 / square.size
      grid(square.grid, (point zip square.center).map((c, sc) => (c - sc + square.size / 2) * z))

    def pattern(ps: PatternSquare, point: Vector[Double]) =
      ps.squares.zipWithIndex.find((s, _) => inSquare(s, point)) match
        case None => Vector.fill(point.size + 1)(-1)
        case Some(s, i) => Vector(i) ++ patternIntern(s, point)

    def patternDensity(ps: PatternSquare, p: Vector[Int]) =
      p.head match
        case -1 => patternDensityForRemaining(ps)
        case i => patternDensityForSquare(ps.squares(i))

    def volume(square: Square) = math.pow(square.size, square.dimension)

    def patternDensityForSquare(square: Square) = volume(square) / math.pow(square.grid, square.dimension)

    def patternDensityForRemaining(patternSquare: PatternSquare) = 1.0 - patternSquare.squares.map(volume).sum

    def allPatterns2D(patternSquare: PatternSquare): Vector[Vector[Int]] =
      Vector(Vector(-1, -1, -1)) ++ {
        for
          (s, i) <- patternSquare.squares.zipWithIndex
          x <- 0 until s.grid
          y <- 0 until s.grid
        yield Vector(i, x, y)
      }

  case class PatternSquare(squares: PatternSquare.Square*)

  @main def patternSquareBenchmark =
    val square = PatternSquare(
      PatternSquare.Square(Vector(0.5, 0.5), 0.01, 10),
      PatternSquare.Square(Vector(0.25, 0.25), 0.01, 10),
      PatternSquare.Square(Vector(0.25, 0.75), 0.01, 10),
      PatternSquare.Square(Vector(0.75, 0.25), 0.01, 10),
      PatternSquare.Square(Vector(0.75, 0.75), 0.01, 10)
    )

    val genomeSize = 2
    val lambda = 100
    val generations = 2000
    val maxRareSample = 10
    val regularisationEpsilon = 1e-6

    val allPatterns = PatternSquare.allPatterns2D(square)

    def trace(s: ppse.StepInfo) =
      if s.generation % 100 == 0
      then
        val all = allPatterns.toSet
        val indexPattern = all.map(k => k -> s.likelihoodRatioMap.getOrElse(k, 0.0)).toMap
        val missed = allPatterns.size - s.likelihoodRatioMap.count((k, _) => all.contains(k))

        val error =
          val sum = indexPattern.values.sum
          val normalized = indexPattern.view.mapValues(_ / sum)
          val (p, q) = normalized.toSeq.map((p, d) => (PatternSquare.patternDensity(square, p), d)).unzip
          jeffreysDivergence(p, q)

        println(s"${s.generation} $error $missed")

    val pdf = ppse.evolution(
      genomeSize = genomeSize,
      lambda = lambda,
      generations = generations,
      maxRareSample = maxRareSample,
      regularisationEpsilon = regularisationEpsilon,
      pattern = PatternSquare.pattern(square, _),
      random = tool.toJavaRandom(org.apache.commons.math3.random.Well44497b(42)),
      trace = trace)

  //  println(PatternSquare.allPatterns2D(square).size)
  //  println(pdf.size)
  //
  //  for (pattern, density) <- pdf
  //  do println(s"${pattern.mkString(",")},$density")


  //@main def benchmark =
  //  import java.util.Random
  //
  //  def pattern(x: Vector[Double], g: Vector[Int]): Vector[Int] =
  //    def pow(p: Vector[Double]): Vector[Double] = p.map(math.pow(_, 4.0))
  //    pow(x) zip g map { (f, g) => math.floor(f * g).toInt }
  //
  //  def powDensity(base: Vector[Double], size: Vector[Double], p: Double = 4.0) =
  //    def antecedent(x: Double) = math.pow(x, 1 / p)
  //
  //    (antecedent(base(0) + size(0)) - antecedent(base(0))) * (antecedent(base(1) + size(1)) - antecedent(base(1)))
  //
  //  def powDensityMap(division: Vector[Int], p: Double = 4.0) =
  //    val sx = 1.0 / division(0)
  //    val sy = 1.0 / division(1)
  //
  //    def densities =
  //      for
  //        (x, i) <- (BigDecimal(0.0) until 1.0 by sx).zipWithIndex
  //        (y, j) <- (BigDecimal(0.0) until 1.0 by sy).zipWithIndex
  //      yield
  //        Vector(i, j) -> powDensity(Vector(x, y).map(_.toDouble), Vector(sx, sy), p)
  //
  //    densities.toMap
  //
  //  val exactDensityMap = powDensityMap(Vector(50, 50))
  //
  //  val genomeSize = 10
  //  val lambda = 100
  //  val dimension = 2
  //  val generations = 2000
  //  val dilation = 2.0
  //  val maxRareSample = 10
  //  val regularisationEpsilon = 1e-6
  //
  //  val divisions = Vector.fill(dimension)(50)
  //
  //  val pdf = ppse.evolution(
  //    genomeSize = genomeSize,
  //    lambda = lambda,
  //    generations = generations,
  //    maxRareSample = maxRareSample,
  //    regularisationEpsilon = regularisationEpsilon,
  //    pattern = pattern(_, divisions),
  //    random = new Random(42))
  //
  //  println(pdf.size)
  //
  //  for (pattern, density) <- pdf
  //  do println(s"${pattern.mkString(",")},$density")
  //
  //  val spreads =
  //    for (pattern, density) <- pdf
  //    yield
  //      val spread =
  //        val exact = exactDensityMap(pattern)
  //        math.abs(density - exact) / exact
  //      spread
  //
  //  println(spreads.sum / spreads.size)