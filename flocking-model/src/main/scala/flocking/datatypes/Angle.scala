package flocking.datatypes

case class Angle(value: Double):
  def unary_- = Angle(-value)
  def abs = Angle(value.abs)
  def <(a: Angle): Boolean = value < a.value
  def <=(a: Angle): Boolean = value <= a.value
  def >(a: Angle): Boolean = value > a.value
  def >=(a: Angle): Boolean = value >= a.value
  def *(x: Double): Angle = Angle(value * x)
  def +(a: Angle): Angle = Angle(value + a.value)
  def -(a: Angle): Angle = Angle(value - a.value)

  def toDouble() = value
