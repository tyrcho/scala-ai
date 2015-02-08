package games

import ai._
import util.Random.nextInt
import scala.annotation.tailrec
import scala.util.Random
import scala.math._
import grizzled.slf4j.Logging

class AlphaBetaTtt(size: Int, needed: Int) extends TTTGame(size, needed) with AlphaBeta with Logging {
  def heuristicValue(node: State, player: Player) =
    node.board.winner match {
      case Some(p) if p == player =>
        POS_INF
      case Some(p) if p != player =>
        NEG_INF
      case None =>
        node.board.cells.values.filter(_ == node.next).size
    }
}

object AlphaBetaTtt extends App {
  val game = new AlphaBetaTtt(size = 3, needed = 3)
  import game._
  val map = Map(
    (0, 0) -> X,
    (1, 1) -> X)
  var state = TTTState(Board(), X)
  while (state.transitions.nonEmpty) {
    state = game.bestMove(state).to
    println(state.board + "\n\n")
  }
}
