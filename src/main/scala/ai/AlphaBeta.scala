package ai

import math._
import grizzled.slf4j.Logging

trait AlphaBeta extends Logging { self: Game =>
  def bestMove(state: State): Transition = {
    val moves = state.transitions.toList
    val count = moves.size
    val player = state.next
    info(s"evaluating $count moves for $player")
    val valuedMoves = for (m <- moves) yield m -> alphaBetaValue(m, player)
    valuedMoves.sortBy(- _._2).head._1
  }

  private def alphaBetaValue(t: Transition, player: Player) = {
    val v = negamax(t.to, t.to.next)
    val value = if (player == t.from.next) -v else v
    debug(s"$value for $player \n$t")
    value
  }

  def heuristicValue(node: State, player: Player): Double

  def negamax(
    node: State,
    player: Player,
    depth: Int = 15,
    alphaIni: Double = NEG_INF,
    betaIni: Double = POS_INF): Double = {
    val transitions = node.transitions
    if (depth == 0 || transitions.isEmpty)
      heuristicValue(node, player)
    else {
      var alpha = alphaIni
      var best = NEG_INF
      var break = false
      val it = transitions.toIterator
      while (!break && it.hasNext) {
        val t = it.next
        val v = -negamax(t.to, t.to.next, depth - 1, -betaIni, -alpha)
        best = max(best, v)
        alpha = max(alpha, v)
        break = betaIni <= alpha
      }
      best
    }
  }

  val POS_INF = 1000.0
  val NEG_INF = -1000.0
}