package flocking.datatypes

import flocking.datatypes

import scala.math.*
import flocking.tools.*
import flocking.tools.DoubleBoundsKeeper

object HeadingBoundsKeeper:
  val hbk = DoubleBoundsKeeper(0,2 * Pi)
  def apply(x:Double) = hbk(x)

case class Heading(value: Double):
  def -(a: Angle): Heading = Heading(value - a.value)
  def +(a: Angle): Heading = Heading(value + a.value)
  def *(d: Double): Heading = Heading(value * d)
  def /(d: Double): Heading = Heading(value / d)
  def angleTo(h:Heading): Angle =
    if (h.value > value)
      if (abs(h.value - value) < abs(h.value - 2*Pi - value)) Angle(h.value - value)
      else Angle(h.value - 2*Pi - value)
    else
    if (abs(h.value + 2*Pi - value) < abs(h.value - value)) Angle(h.value + 2*Pi - value)
    else Angle(h.value - value)

  def toDouble: Double = value

object Heading:
  def fromDouble(value: Double): Heading = datatypes.Heading(HeadingBoundsKeeper(value))
