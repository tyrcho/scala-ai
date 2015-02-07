package games

import ai._
import util.Random.nextInt
import scala.annotation.tailrec
import scala.util.Random
import scala.math._

case object X extends Player { val opponent = O }
case object O extends Player { val opponent = X }
sealed trait Player {
  def opponent: Player
}

private object Player {
  def opponent(player: Option[Player]): Option[Player] =
    player.map(_.opponent)
}

case class TTTGame(size: Int, needed: Int) extends UctGame with Game {

  object Board {
    type Cells = Map[(Int, Int), Player]
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
        case None    => "."
        case Some(p) => p.toString
      }
    }).mkString).
      mkString("\n")
  }

  type State = TTTState
  type Transition = TTTTransition
  type SearchPolicy = TTTSearchPolicy.type
  type StateValue = Option[Player]

  case class TTTState(board: Board, player: Player) extends GameState {
    override lazy val transitions: Seq[TTTTransition] =
      board.legalPlays.map {
        case (i, j) => new TTTTransition(this, player, i, j)
      }

    override lazy val value: Option[Player] = board.winner
  }

  case class TTTTransition(from: TTTState, player: Player, i: Int, j: Int) extends GameTransition {
    lazy val to =
      TTTState(from.board.play(player, i, j), player.opponent)
  }

  object TTTSearchPolicy extends GameSearchPolicy {
    override def normalize(value: Option[Player], state: TTTState): Double = value match {
      case None                 => 0.5
      case Some(player: Player) => if (player == state.player) 1 else 0
    }

    override def stochasticTransition(state: State): Transition =
      stochasticHelper(state, weight)

    def weight(t: Transition) = sqr(size / 2 + 1) - sqr(t.i - size / 2) - sqr(t.j - size / 2)

    def sqr(i: Int) = i * i
  }
}

object MainTTT extends App {
  val game = TTTGame(size = 6, needed = 4)
  import game._
  var state = TTTState(Board(), X)
  while (state.transitions.nonEmpty) {
    val root = Node(state, TTTSearchPolicy)
    val start = System.nanoTime
    val delay = 1000 * 1000 * 1000 // 1000 ms
    var count = 0
    while (System.nanoTime < start + delay) {
      root.value
      count += 1
    }
    state = root.asInstanceOf[BanditNode].bestTransition.to
    println(s"$count simulations")
    println(state.board + "\n---")
  }
}
