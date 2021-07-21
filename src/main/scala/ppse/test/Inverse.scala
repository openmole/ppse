package ppse.test

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, EigenDecomposition}

/*
 * Copyright (C) 2021 Romain Reuillon
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

object Inverse extends App {

  val m = Array(Array(2.598098549137219E-4, -6.413946636779064E-4), Array(-6.413946636779064E-4,  0.0015834161284263403))

  val covarianceMatrix = new Array2DRowRealMatrix(m)

  // Covariance matrix eigen decomposition.
  val covMatDec = new EigenDecomposition(covarianceMatrix)

  // Compute and store the inverse.
  val covarianceMatrixInverse = covMatDec.getSolver.getInverse

}
