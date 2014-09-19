package games

import ai._
import util.Random.nextInt
import scala.annotation.tailrec

sealed trait Player {
  def opponent: Player
}

private object Player {
  def opponent(player: Option[Player]): Option[Player] =
    player.map(_.opponent)
}

case object X extends Player { val opponent = O }
case object O extends Player { val opponent = X }

object Board {
  type Cells = Map[(Int, Int), Player]
  class IllegalPlayException extends Throwable
}

case class Board(size: Int, needed: Int, cells: Board.Cells = Map.empty) {

  private lazy val emptyCells: Seq[(Int, Int)] =
    for {
      i <- 0 until size
      j <- 0 until size
      if !cells.isDefinedAt(i, j)
    } yield (i, j)

  lazy val legalPlays: Seq[(Int, Int)] =
    if (winner.nonEmpty) Nil else emptyCells

  def play(player: Player, i: Int, j: Int): Board = {
    if (!legalPlays.contains((i, j)))
      throw new Board.IllegalPlayException
    copy(cells = cells.updated((i, j), player))
  }

  lazy val winner: Option[Player] = {
    for {
      (dx, dy) <- Seq((0, 1), (1, 0), (1, 1), (1, -1))
      ((x, y), p) <- cells
      if isWin(p, x, y, dx, dy)
    } yield p
  }.headOption

  @tailrec
  private def isWin(p: Player, x: Int, y: Int, dx: Int, dy: Int, found: Int = 0): Boolean = {
    found == needed ||
      (cells.get((x, y)) == Some(p) &&
        isWin(p, x + dx, y + dy, dx, dy, found + 1))
  }

  override def toString = (for {
    i <- 0 until size
  } yield (for {
    j <- 0 until size
  } yield {
    cells.get((i, j)) match {
      case None => "."
      case Some(p) => p.toString
    }
  }).mkString).
    mkString("\n")
}

case class TTTState(board: Board, player: Player) extends State[Option[Player], TTTTransition] {
  override lazy val transitions: Seq[TTTTransition] = board.legalPlays.map { case (i, j) => new TTTTransition(this, player, i, j) }
  override lazy val value: Option[Player] = board.winner
}

case class TTTTransition(from: TTTState, player: Player, i: Int, j: Int) extends Transition[TTTState] {
  lazy val to = TTTState(from.board.play(player, i, j), player.opponent)
}

object TTTSearchPolicy extends SearchPolicy[Option[Player], TTTState, TTTTransition] {
  override def stochasticTransition(state: TTTState): TTTTransition = state.transitions(nextInt(state.transitions.size))
  override def normalize(value: Option[Player], state: TTTState): Double = value match {
    case None => 0.5
    case Some(player: Player) => if (player == state.player) 1 else 0
  }
}

object MainTTT extends App {
  var state = new TTTState(Board(6,4), X)
  while (state.transitions.nonEmpty) {
    val root = Node(state, TTTSearchPolicy)
    for (i <- 1 to 10*1000)
      root.value
    state = root.asInstanceOf[BanditNode[_, _, TTTTransition]].bestTransition.to
    println(state.board + "\n---")
  }
}
