package flocking.tools

//---- Base types ----//
case class DoubleBoundsKeeper(lowBound: Double, highBound: Double):

  def keepInBounds(x: Double): Double =
    if (x >= highBound) ((x - lowBound) % (highBound - lowBound)) + lowBound
    else if (x < lowBound) highBound - ((highBound - x) % (highBound - lowBound))
        else x

  def apply(x: Double): Double = keepInBounds(x)


