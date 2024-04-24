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

  def buildRejectionSampler(gmm: GMM, rng: Random) =
    val distribution = GMM.toDistribution(gmm, rng)

    def sample() =
      val x = distribution.sample()
      (x, distribution.density(x))

    def accept(p: Array[Double]) = p.forall(_ >= 0.0) && p.forall(_ <= 1.0)
    RejectionSampler(sample, accept)

  case class RejectionSamplerState(test: Long = 0L, pass: Long = 0L):
    def inverseProbability() = test.toDouble / pass

  object RejectionSampler:
    def success(state: RejectionSamplerState) = RejectionSamplerState(state.test + 1, state.pass + 1)
    def fail(state: RejectionSamplerState) = RejectionSamplerState(state.test + 1, state.pass)
    def allFailed(state: RejectionSamplerState) = state.pass == 0L

  case class RejectionSampler(sampleFunction: () => (Array[Double], Double), accept: Array[Double] => Boolean)

  def warmupSampler(sampler: RejectionSampler, n: Int, state: RejectionSamplerState = RejectionSamplerState()): RejectionSamplerState =
    if n > 0
    then
      val (x, _) = sampler.sampleFunction()
      if (!sampler.accept(x)) warmupSampler(sampler, n - 1, RejectionSampler.fail(state))
      else warmupSampler(sampler, n - 1, RejectionSampler.success(state))
    else state

  def sample(sampler: RejectionSampler, state: RejectionSamplerState = RejectionSamplerState()): (RejectionSamplerState, (Array[Double], Double)) =
    val (x, density) = sampler.sampleFunction()
    if !sampler.accept(x)
    then sample(sampler, RejectionSampler.fail(state))
    else
      val newState = RejectionSampler.success(state)
      (newState, (x, density / newState.inverseProbability()))

  def sampleArray(sampler: RejectionSampler, n: Int, state: RejectionSamplerState = RejectionSamplerState(), res: List[(Array[Double], Double)] = List()): (RejectionSamplerState, Array[(Array[Double], Double)]) =
    if n > 0
    then
      val (newState, newSample) = sample(sampler, state)
      sampleArray(sampler, n - 1, newState, newSample :: res)
    else (state, res.reverse.toArray)
