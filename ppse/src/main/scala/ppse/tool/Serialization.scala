package ppse.tool

import ppse.em.GMM
import java.io.File
import better.files.*

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

object Serialization:
  case class PPSEDrawState(evaluation: Long, point: Vector[Vector[Double]], gmm: Option[GMM])
  case class PPSEEvolution(states: Seq[PPSEDrawState])

  def save(s: PPSEEvolution, f: File) =
    import io.circe.*
    import io.circe.generic.auto.*
    import io.circe.syntax.*

    f.toScala.delete(swallowIOExceptions = true)
    f.toScala.write(s.asJson.noSpaces)

  def load(f: File): PPSEEvolution =
    import io.circe.*
    import io.circe.generic.auto.*
    import io.circe.syntax.*

    parser.parse(f.toScala.contentAsString).toTry.get.as[PPSEEvolution].toTry.get
//def save()

