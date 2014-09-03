package games

import ai._
import util.Random.nextInt

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
  type Cells = Vector[Option[Player]]
  type Group = (Option[Player], Option[Player], Option[Player])
  class IllegalPlayException extends Throwable
}

case class Board(val cells: Board.Cells = Vector.fill(9)(None)) {

  private lazy val emptyCells: Seq[Int] =
    (0 until 9).filter(cells(_).isEmpty)

  private lazy val groups: Traversable[Board.Group] = List(
    (cells(0), cells(1), cells(2)),
    (cells(3), cells(4), cells(5)),
    (cells(6), cells(7), cells(8)),
    (cells(0), cells(3), cells(6)),
    (cells(1), cells(4), cells(7)),
    (cells(2), cells(5), cells(8)),
    (cells(0), cells(4), cells(8)),
    (cells(2), cells(4), cells(6)))

  lazy val legalPlays: Seq[Int] =
    if (winner.nonEmpty) Nil else emptyCells

  def play(player: Player, index: Int): Board = {
    if (legalPlays.find(index.equals) == None)
      throw new Board.IllegalPlayException
    new Board(cells.updated(index, Some(player)))
  }

  lazy val winner: Option[Player] = groups.find(g => g._1 == g._2 && g._2 == g._3 && g._1 != None) match {
    case None => None
    case Some(group) => group._1
  }

  override def toString = {
    val cells: Seq[String] = this.cells.map(_ match {
      case Some(player) => player.toString
      case None => " "
    })
    "%s%s%s\n%s%s%s\n%s%s%s".format(cells: _*)
  }
}

case class TTTState(board: Board, player: Player) extends State[Option[Player], TTTTransition] {
  override lazy val transitions: Seq[TTTTransition] = board.legalPlays.map(new TTTTransition(this, player, _))
  override lazy val value: Option[Player] = board.winner
}

case class TTTTransition(from: TTTState, player: Player, index: Int) extends Transition[TTTState] {
  lazy val to = TTTState(from.board.play(player, index), player.opponent)
}

object TTTSearchPolicy extends SearchPolicy[Option[Player], TTTState, TTTTransition] {
  override def stochasticTransition(state: TTTState): TTTTransition = state.transitions(nextInt(state.transitions.size))
  override def normalize(value: Option[Player], state: TTTState): Double = value match {
    case None => 0.5
    case Some(player: Player) => if (player == state.player) 1 else 0
  }
}

object MainTTT extends App {
  var state = new TTTState(new Board, X)
  while (state.transitions.nonEmpty) {
    val root = Node(state, TTTSearchPolicy)
    for (i <- 1 to 1000)
      root.value
    state = root.asInstanceOf[BanditNode[_, _, TTTTransition]].bestTransition.to
    println(state.board + "\n---")
  }
}
