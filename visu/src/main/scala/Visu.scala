/*
 * Copyright (C) 2023 Romain Reuillon
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

import org.scalajs.dom.*

@main def visu(c: html.Canvas): Unit =
  println("Hello world!")

  type Ctx2D = CanvasRenderingContext2D
  val ctx = c.getContext("2d")
    .asInstanceOf[Ctx2D]
  val w = 300
  c.width = w
  c.height = w

  ctx.strokeStyle = "red"
  ctx.lineWidth = 3
  ctx.beginPath()
  ctx.moveTo(w / 3, 0)
  ctx.lineTo(w / 3, w / 3)
  ctx.moveTo(w * 2 / 3, 0)
  ctx.lineTo(w * 2 / 3, w / 3)
  ctx.moveTo(w, w / 2)
  ctx.arc(w / 2, w / 2, w / 2, 0, 3.14)

  ctx.stroke()
