package ai

import math._
import grizzled.slf4j.Logging

trait AlphaBeta extends Logging { self: Game =>
  def bestMove(state: State, depth: Int = 3): Transition = {
    val moves = state.transitions.toList
    val count = moves.size
    val player = state.next
    info(s"evaluating $count moves for $player")
    val valuedMoves = for (m <- moves) yield m -> alphaBetaValue(m, player, depth)
    valuedMoves.sortBy(-_._2).head._1
  }

  private def alphaBetaValue(t: Transition, player: Player, depth: Int) = {
    val v = negamax(t.to, t.to.next, depth)
    val value = if (player == t.from.next) -v else v
    debug(s"$value for $player \n$t")
    value
  }

  def heuristicValue(node: State, player: Player): Double

  def negamax(
    node: State,
    player: Player,
    depth: Int = 3,
    alphaIni: Double = Double.MinValue,
    betaIni: Double = Double.MaxValue): Double = {
    val transitions = node.transitions
    if (depth == 0 || transitions.isEmpty)
      heuristicValue(node, player)
    else {
      val (_, best) = transitions.foldLeft((alphaIni, Double.MinValue)) {
        case ((a, b), t) =>
          if (betaIni <= a)
            (a, b)
          else {
            val v = -negamax(t.to, t.to.next, depth - 1, -betaIni, -a)
            (max(a, v), max(b, v))
          }
      }
      debug(s"$best is the score for $player in $node")
      best
    }
  }

}