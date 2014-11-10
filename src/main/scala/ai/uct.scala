package ai

import ai.ucb._
import collection.mutable.{ Map, HashMap }
import util.Random.shuffle
import scala.util.Random
import scala.annotation.tailrec

trait Game {
  type State <: GameState
  type Transition <: GameTransition
  type SearchPolicy <: GameSearchPolicy
  type StateValue

  trait GameState {
    def transitions: Traversable[Transition]
    def value: StateValue
  }

  trait GameTransition {
    def from: State
    def to: State
  }

  trait GameSearchPolicy {
    def stochasticTransition(state: State): Transition = shuffle(state.transitions).head
    def normalize(value: StateValue, state: State): Double

    def stochasticHelper(from: State, weight: Transition => Int): Transition = {
      val weights = for (t <- from.transitions) yield t -> weight(t)
      sample(weights)
    }

    def sample[T](weighted: Traversable[(T, Int)]): T = {
      @tailrec
      def recurse(rest: Int, elements: Traversable[(T, Int)]): T = {
        val (elt, weight) = elements.head
        if (weight >= rest) elt else recurse(rest - weight, elements.tail)
      }

      recurse(weighted.map(_._2).sum, weighted)
    }
  }

  sealed trait Node {
    def state: State
    def value: StateValue
  }

  object Node {
    def apply(
      state: State,
      policy: SearchPolicy,
      nodes: Map[State, Node]): Node =
      nodes.getOrElseUpdate(state,
        if (state.transitions.nonEmpty)
          new BanditNode(state, policy, nodes)
        else
          new TerminalNode(state))

    def apply(state: State, policy: SearchPolicy): Node =
      apply(state, policy, new HashMap[State, Node])
  }

  case class BanditNode(state: State,
    policy: SearchPolicy,
    nodes: Map[State, Node]) extends Node {

    private val arms = state.transitions.map(new Edge(_, policy, nodes))

    private var bandit = new Bandit[StateValue, Edge](arms, policy.normalize(_, state))

    // has side effects :(
    def value: StateValue = bandit.play match {
      case (value, bandit) => this.bandit = bandit; value
    }

    def bestTransition = bandit.bestArm.transition
  }

  case class TerminalNode(state: State) extends Node {
    def value = state.value
  }

  case class Edge(
    transition: Transition,
    policy: SearchPolicy,
    nodes: Map[State, Node]) extends Arm[StateValue] {

    private lazy val to = Node(transition.to, policy, nodes)
    private var initialized = false

    private def playStochastic(state: State): StateValue =
      if (state.transitions.nonEmpty)
        playStochastic(policy.stochasticTransition(state).to)
      else
        state.value

    // has side effects :(
    def apply: StateValue =
      if (initialized)
        to.value
      else {
        initialized = true
        playStochastic(transition.to)
      }
  }
}