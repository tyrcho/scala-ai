package ai

import scala.util.Random._
import scala.annotation.tailrec

trait Game {
  type State <: GameState
  type Transition <: GameTransition
  type SearchPolicy <: GameSearchPolicy
  type StateValue
  type Player

  trait GameState {
    def transitions: Traversable[Transition]
    def value: StateValue
    def next: Player
  }

  trait GameTransition {
    def from: State
    def to: State
  }

  trait GameSearchPolicy {
    def stochasticTransition(state: State): Transition =
      shuffle(state.transitions).head

    def normalize(value: StateValue, state: State): Double

    def stochasticHelper(from: State, weight: Transition => Int): Transition = {
      val weights = for (t <- from.transitions) yield t -> weight(t)
      sample(weights)
    }

    def sample(weighted: Traversable[(Transition, Int)]): Transition = {
      @tailrec
      def recurse(rest: Int, elements: Traversable[(Transition, Int)]): Transition = {
        val (elt, weight) = elements.head
        if (weight >= rest) elt else recurse(rest - weight, elements.tail)
      }

      recurse(weighted.map(_._2).sum, weighted)
    }
  }

}