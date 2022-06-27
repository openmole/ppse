package ppse.tool


object RejectionSampler {

  /**
   * Monte Carlo estimation of the success rate of the predicate.
   *
   * @param test
   * @param pass
   */
  case class State(test: Long = 0L, pass: Long = 0L) {
    def inverseProbability() = test.toDouble / pass
  }

}


import RejectionSampler._
import shapeless.Lazy

import scala.annotation.tailrec
/**
 * Rejection sampler with a predicate and a state.
 *
 * @param dist
 * @param patternFunction
 * @param accept
 */
class RejectionSampler(_sample: () => (Vector[Double], Lazy[Double]), val accept: Vector[Double] => Boolean) {
  def success(state: State) = State(state.test + 1, state.pass + 1)
  def fail(state: State) = State(state.test + 1, state.pass)

  def warmup(n: Int, state: State = State()): State =
    if(n > 0) {
      val (x, _) = _sample()
      if (!accept(x)) warmup(n - 1, fail(state))
      else warmup(n - 1, success(state))
    } else state

  def sample(state: State = State()): (State, (Vector[Double], Double)) = {
    @tailrec def sample0(state: State): (State, (Vector[Double], Double)) = {
      val (x, density) = _sample()
      if (!accept(x)) {
        // if the sample is rejected, resample and keep the failure in the state
        sample0(fail(state))
      } else {
        val newState = success(state)
        // if the sample is accepted, return the state, the sample pattern and the adjusted density
        (newState, (x, density.value / newState.inverseProbability()))
      }
    }

    sample0(state)
  }

  @tailrec final def sampleVector(n: Int, state: State = State(), res: List[(Vector[Double], Double)] = List()): (State, Vector[(Vector[Double], Double)]) = {
    if(n > 0) {
      val (newState, newSample) = sample(state)
      sampleVector(n - 1, newState, newSample :: res)
    } else (state, res.reverse.toVector)
  }

}
