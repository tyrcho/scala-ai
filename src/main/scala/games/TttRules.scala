package games

import ai.Game
import scala.annotation.tailrec

case object X extends TttPlayer { val opponent = O }
case object O extends TttPlayer { val opponent = X }
sealed trait TttPlayer {
  val opponent: TttPlayer
}

private object TttPlayer {
  def opponent(TttPlayer: Option[TttPlayer]): Option[TttPlayer] =
    TttPlayer.map(_.opponent)
}

case class TTTGame(size: Int, needed: Int) extends Game {

  object Board {
    type Cells = Map[(Int, Int), TttPlayer]
    class IllegalPlayException extends Throwable
  }

  case class Board(cells: Board.Cells = Map.empty) {
    private lazy val emptyCells: Seq[(Int, Int)] =
      for {
        i <- 0 until size
        j <- 0 until size
        if !cells.isDefinedAt(i, j)
      } yield (i, j)

    lazy val legalPlays: Seq[(Int, Int)] =
      if (winner.nonEmpty) Nil else emptyCells

    def play(player: TttPlayer, i: Int, j: Int): Board = {
      if (!legalPlays.contains((i, j)))
        throw new Board.IllegalPlayException
      copy(cells = cells.updated((i, j), player))
    }

    lazy val winner: Option[TttPlayer] = {
      for {
        (dx, dy) <- Seq((0, 1), (1, 0), (1, 1), (1, -1))
        ((x, y), p) <- cells
        if isWin(p, x, y, dx, dy)
      } yield p
    }.headOption

    @tailrec
    private def isWin(p: TttPlayer, x: Int, y: Int, dx: Int, dy: Int, found: Int = 0): Boolean = {
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
        case None    => "."
        case Some(p) => p.toString
      }
    }).mkString).
      mkString("\n")
  }

  type State = TTTState
  type Transition = TTTTransition
  type SearchPolicy = TTTSearchPolicy.type
  type StateValue = Option[TttPlayer]
  type Player = TttPlayer

  case class TTTState(board: Board, next: TttPlayer, moves: Int = 0) extends GameState {
    override lazy val transitions: Seq[TTTTransition] =
      board.legalPlays.map {
        case (i, j) => TTTTransition(this, next, i, j)
      }

    override lazy val value: Option[TttPlayer] = board.winner

    override def toString = board.toString + s"($next)"
  }

  case class TTTTransition(from: TTTState, player: TttPlayer, i: Int, j: Int) extends GameTransition {
    lazy val to =
      TTTState(from.board.play(player, i, j), player.opponent, from.moves + 1)
  }

  object TTTSearchPolicy extends GameSearchPolicy {
    override def normalize(value: Option[TttPlayer], state: TTTState): Double = value match {
      case None                    => 0.5
      case Some(player: TttPlayer) => if (player == state.next) 1 else 0
    }

    override def stochasticTransition(state: State): Transition =
      stochasticHelper(state, weight)

    def weight(t: Transition) = sqr(size / 2 + 1) - sqr(t.i - size / 2) - sqr(t.j - size / 2)

    def sqr(i: Int) = i * i
  }
}

