package ppse.tool

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

def multinomialDraw[T](s: Vector[(Double, T)], n: Int, rng: util.Random, replacement: Boolean = false): List[T] = {
  assert(!s.isEmpty, "Input sequence should not be empty")
  if !replacement then assert(n <= s.size)

  def select(remaining: List[(Double, T)], value: Double, begin: List[(Double, T)] = List.empty): (T, List[(Double, T)]) =
    remaining match
      case (weight, e) :: tail =>
        if (value <= weight)
          if !replacement then (e, begin.reverse ::: tail) else (e, s.toList)
        else select(tail, value - weight, (weight, e) :: begin)
      case _ => sys.error(s"Bug $remaining $value $begin")


  def drawN(n: Int, list: List[(Double, T)], acc: List[T] = List.empty): List[T] =
    if n == 0
    then acc
    else
      val totalWeight = list.unzip._1.sum
      val (t, l) = select(list, rng.nextDouble * totalWeight)
      drawN(n - 1, l, t ::acc)

  drawN(n, s.toList)
}
