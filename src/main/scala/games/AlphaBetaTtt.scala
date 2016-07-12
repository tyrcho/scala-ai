package games

import ai._
import util.Random.nextInt
import scala.annotation.tailrec
import scala.util.Random
import scala.math._
import grizzled.slf4j.Logging

class AlphaBetaTtt(size: Int, needed: Int) extends TTTGame(size, needed) with AlphaBeta with Logging {

  def heuristicValue(node: State, player: Player) = {
    val board = node.board
    node.board.winner match {
      case Some(p) if p == player =>
        debug(s"$p wins in $board")
        1000 - node.moves // when all is lost, try to last longer
      case Some(p) if p != player =>
        debug(s"$p wins in $board so $player lost")
        -1000 + node.moves
      case None => value(node.board, player)
    }
  }

  private def value(b: Board, p: Player) = {
    val v = {
      for {
        ((i, j), pl) <- b.cells
        sign = if (p == pl) -1 else 1
      } yield sign * (sqr(i - size / 2) + sqr(j - size / 2))
    }.sum
    debug(s"$v is estimation for $b")
    v
  }

  def sqr(n: Double) = n * n
}

object AlphaBetaTtt extends App {
  val game = new AlphaBetaTtt(size = 6, needed = 4)
  import game._
  val map = Map(
    (0, 0) -> X,
    (1, 1) -> X)
  var state = TTTState(Board(), O)
  println(state.board + "\n\n")
  while (state.transitions.nonEmpty) {
    val move = game.bestMove(state, 3)
    state = move.to
    println(state.board + "\n\n")
  }
}
