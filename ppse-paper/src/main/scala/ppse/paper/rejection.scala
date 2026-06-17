package ppse.paper


/*
 * Copyright (C) 2024 Romain Reuillon
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

import java.util.Random

object rejection:

  object RejectionSampler:

    def success(state: RejectionSamplerState) = RejectionSamplerState(state.test + 1, state.pass + 1)
    def fail(state: RejectionSamplerState) = RejectionSamplerState(state.test + 1, state.pass)
    def allFailed(state: RejectionSamplerState) = state.pass == 0L

    def warmup(sampler: RejectionSampler, n: Int, state: RejectionSamplerState = RejectionSamplerState()): RejectionSamplerState =
      if n > 0
      then
        val x = sampler.sampleFunction()
        if !sampler.accept(x)
        then warmup(sampler, n - 1, RejectionSampler.fail(state))
        else warmup(sampler, n - 1, RejectionSampler.success(state))
      else state

    def sample(sampler: RejectionSampler, state: RejectionSamplerState = RejectionSamplerState()): (RejectionSamplerState, Array[Double]) =
      val x = sampler.sampleFunction()
      if !sampler.accept(x)
      then sample(sampler, RejectionSampler.fail(state))
      else
        val newState = RejectionSampler.success(state)
        (newState, x)

    def density(state: RejectionSamplerState, d: Double) =
      val inverseProbability = state.test.toDouble / state.pass
      d / inverseProbability


  case class RejectionSampler(sampleFunction: () => Array[Double], accept: Array[Double] => Boolean)
  case class RejectionSamplerState(test: Long = 0L, pass: Long = 0L)

