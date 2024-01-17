package flocking.datatypes

import flocking.tools.*

case class Point(x: Double, y: Double):
  override def toString: String = super.toString ++ s":($x, $y)"


case class PointBoundsKeeper(lowBound: Point, highBound: Point):
  val bkAbscissa = DoubleBoundsKeeper(lowBound.x, highBound.x)
  val bkOrdinate = DoubleBoundsKeeper(lowBound.y, highBound.y)
  def apply(p:Point): Point = new Point(bkAbscissa(p.x), bkOrdinate(p.y))
