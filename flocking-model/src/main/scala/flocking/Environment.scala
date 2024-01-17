package flocking

import flocking.datatypes.Point
import flocking.datatypes.*
import flocking.tools.*
import flocking.tools.{Distance, DoubleBoundsKeeper}

import scala.reflect.ClassTag


case class Environment(
  nCellsWide: Int,
  nCellsHigh: Int,
  width: Double,
  height: Double,
  private var pixels: Array[Int],
  emptySpace: Int):

  lazy val widthBoundsKeeper = DoubleBoundsKeeper(0, width)
  lazy val heightBoundsKeeper = DoubleBoundsKeeper(0, height)

  private def set(i: Int, j: Int, v: Int) = pixels(j * nCellsWide + i) = v
  def get(i: Int, j: Int): Int = pixels(j * nCellsWide + i)
  // def get(x: Double, y: Double): T = {
  //   val i = if (x == width) nCellsWide - 1 else x2i(x)
  //   val j = if (y == height) nCellsWide - 1 else y2j(y)
  //   get(i,j)
  // }
  private def set(x: Double, y: Double, v: Int): Unit = set(x2i(widthBoundsKeeper(x)),y2j(heightBoundsKeeper(y)), v)
  def get(x: Double, y: Double): Int = get(x2i(widthBoundsKeeper(x)),y2j(heightBoundsKeeper(y)))
  //def getTorus(x: Double, y: Double): T =
  // def getNormal(x: Double, y:Double): T = {
  //   val i = if (x == 1.0) nCellsWide - 1 else x2i(x)
  //   val j = if (y == 1.0) nCellsWide - 1 else y2j(y)
  //   get(i,j)
  // }

  /** returns the discreete coordinate (cell position) corresponding to the real coordinate given */
  def x2i(x: Double): Int = ((x / width) * nCellsWide).toInt
  def y2j(y: Double): Int = ((y / height) * nCellsHigh).toInt

  def i2x(i: Int): Double = (((i:Double) / nCellsWide) * width) + (width / nCellsWide / 2.0)
  def j2y(j: Int): Double = (((j:Double) / nCellsHigh) * height) + (height / nCellsHigh / 2.0)

  private def addDisc(d: Environment.Disc) =
    val center = Point(d.x, d.y)
    for
      i <- 0 until nCellsWide
      j <- 0 until nCellsHigh
      if Distance.torus(width, height)(center, Point(i2x(i),j2y(j))) <= d.r
    do set(i,j, d.fill)


object Environment:

  case class Disc(x:Double, y:Double, r:Double, fill: Int)

  def isEmpty(environment: Environment, x: Double, y: Double) = Environment.get(environment, x, y) == emptySpace(environment)
  def emptySpace(environment: Environment) = environment.emptySpace
  def get(environment: Environment, x: Double, y: Double) = environment.get(x, y)

  def empty(width: Double, height: Double) = apply(0, 1, 1, width, height)

  def apply(f :Int, nCellsWide: Int, nCellsHigh: Int, width: Double, height: Double, disc: Seq[Disc] = Seq()) =
    val env =
      new Environment(
        nCellsWide = nCellsWide,
        nCellsHigh = nCellsHigh,
        width = width,
        height = height,
        pixels = Array.fill(nCellsHigh * nCellsWide)(f),
        emptySpace = f
      )
    disc.foreach(env.addDisc)
    env


