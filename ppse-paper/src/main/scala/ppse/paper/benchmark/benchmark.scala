package ppse.paper.benchmark



/*
 * Copyright (C) 2022 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


import ppse.paper.*

def normalise(p: Seq[Double]) =
  val sum = p.sum
  p.map(_ / sum)

def kullbackLeiblerDivergence(p: Seq[Double], q: Seq[Double], epsilon: Option[Double] = Some(1e-10)): Double =
  val epsilonValue = epsilon.map(e => Math.max(0, e)).getOrElse(0.0)
  (normalise(p) zip normalise(q)).map: (x, y) =>
    val yValue = Math.max(epsilonValue, y)
    if yValue == 0 || x == 0
    then 0
    else x * math.log(x / yValue)
  .sum

def jeffreysDivergence(p: Seq[Double], q: Seq[Double], epsilon: Option[Double] = Some(1e-10)): Double =
  kullbackLeiblerDivergence(p, q, epsilon) + kullbackLeiblerDivergence(q, p, epsilon)
