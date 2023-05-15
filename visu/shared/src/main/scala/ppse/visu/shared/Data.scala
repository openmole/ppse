package ppse.visu.shared

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

object Data {
  case class Foo(bar: Int)

  type PointData = Vector[Double]

  case class GMMData(
    means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]],
    weights: Array[Double],
    parameters: Array[EllipseParameters])

  case class RunState(evaluation: Long, gmm: Option[GMMData], point: Seq[PointData])
  case class RunData(states: Seq[RunState])
  //case class CovarianceParameters(meanX: Double, meanY: Double, covariance: Array[Array[Double]])
  case class EllipseParameters(centerX: Double, centerY: Double, radiusX:Double, radiusY: Double, angle: Double)
}
