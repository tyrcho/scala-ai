package ai

import collection.mutable.{Map, HashMap}
import util.Random.shuffle

trait State[V, T <: Transition[_ <: State[V, T]]] {
  def transitions: Traversable[T]
  def value: V
}

trait Transition[S <: State[_, _ <: Transition[S]]] {
  def from: S
  def to: S
}

trait SearchPolicy[V, S <: State[V, T], T <: Transition[S]] {
  def stochasticTransition(state: S): T = shuffle(state.transitions).head
  def normalize(value: V, state: S): Double
}

sealed trait Node[V, S <: State[V, _]] {
  def state: S
  def value: V
}

final object Node {
  def apply[V, S <: State[V, T], T <: Transition[S]]
      (state: S, policy: SearchPolicy[V, S, T], nodes: Map[S, Node[V, S]]): Node[V, S] =
    nodes.getOrElseUpdate(state,
      if (state.transitions.nonEmpty)
        new BanditNode[V, S, T](state, policy, nodes)
      else
        new TerminalNode[V, S](state))

  def apply[V, S <: State[V, T], T <: Transition[S]]
      (state: S, policy: SearchPolicy[V, S, T]): Node[V, S] =
    apply(state, policy, new HashMap[S, Node[V, S]])
}

final case class BanditNode[V, S <: State[V, T], T <: Transition[S]]
    (override val state: S,
     policy: SearchPolicy[V, S, T],
     nodes: Map[S, Node[V, S]]) extends Node[V, S] {
  private val arms = state.transitions.map(new Edge(_, policy, nodes))
  private var bandit = new Bandit[V, Edge[V, S, T]](arms, policy.normalize(_, state))

  override def value: V = bandit.play match {
    case (value, bandit) => this.bandit = bandit; value
  }
  def bestTransition = bandit.bestArm.transition
}

final case class TerminalNode[V, S <: State[V, _]]
    (override val state: S) extends Node[V, S] {
  override def value = state.value
}

final class Edge[V, S <: State[V, T], T <: Transition[S]]
    (val transition: T,
     policy: SearchPolicy[V, S, T],
     nodes: Map[S, Node[V, S]]) extends Arm[V] {

  private lazy val to = Node(transition.to, policy, nodes)
  private var initialized = false

  private def playStochastic(state: S): V =
    if (state.transitions.nonEmpty)
      playStochastic(policy.stochasticTransition(state).to)
    else
      state.value

  override def play: V = {
    if (initialized)
      to.value
    else {
      initialized = true
      playStochastic(transition.to)
    }
  }

}