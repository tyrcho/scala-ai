package ai

import math._
import grizzled.slf4j.Logging

trait AlphaBeta extends Logging { self: Game =>
  def bestMove(state: State): Transition = {
    val moves = state.transitions.toList
    val count = moves.size
    info(s"evaluating $count moves")
    val valuedMoved = for (m <- moves) yield m -> -alphaBetaValue(m, state.next)
    valuedMoved.sortBy(_._2).head._1
  }

  private def alphaBetaValue(t: Transition, player: Player) = {
    val value = alphabeta(t.to, player)
    debug(s"$value for $player \n$t")
    value
  }

  def heuristicValue(node: State, player: Player): Double

  def alphabeta(
    node: State,
    player: Player,
    depth: Int = 15,
    alphaIni: Double = NEG_INF,
    betaIni: Double = POS_INF,
    maximizingPlayer: Boolean = true): Double = {
    val transitions = node.transitions
    if (depth == 0 || transitions.isEmpty) heuristicValue(node, player)
    else if (maximizingPlayer) {
      var alpha = alphaIni
      var v = NEG_INF
      var break = false
      val it = transitions.toIterator
      while (!break && it.hasNext) {
        val t = it.next
        v = max(v, alphabeta(t.to, player, depth - 1, alpha, betaIni, false))
        alpha = max(alpha, v)
        break = betaIni <= alpha
      }
      v
    } else {
      var beta = betaIni
      var v = POS_INF
      var break = false
      val it = transitions.toIterator
      while (!break && it.hasNext) {
        val t = it.next
        v = min(v, alphabeta(t.to, player, depth - 1, alphaIni, beta, true))
        beta = min(beta, v)
        break = beta <= alphaIni
      }
      v
    }
  }

  val POS_INF = Double.MaxValue
  val NEG_INF = Double.MinValue
}