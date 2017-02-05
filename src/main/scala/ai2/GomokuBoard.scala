package ai2

import scala.annotation.tailrec

case class GomokuBoard(
    width: Int,
    height: Int,
    next: Boolean = false,
    playedTrue: Set[Pos] = Set(),
    playedFalse: Set[Pos] = Set(),
    quickFree: Option[Set[Pos]] = None) {

  def play(x: Int, y: Int): GomokuBoard = play(Pos(x, y))

  def play(p: Pos): GomokuBoard =
    if (next)
      copy(next = false, playedTrue = playedTrue + p, quickFree = Some(free - p))
    else
      copy(next = true, playedFalse = playedFalse + p, quickFree = Some(free - p))

  def free: Set[Pos] = quickFree.getOrElse(allFree)

  override def toString = toText

  def toText: String = Seq.tabulate(height) { y =>
    Seq.tabulate(width) { x =>
      if (playedFalse(Pos(x, y))) 'F'
      else if (playedTrue(Pos(x, y))) 'T'
      else ' '
    }.mkString
  }.mkString("\n") + s"\n${next.toString.toUpperCase.head} to play"

  def maxLength(player: Boolean) = {
    val played = if (player) playedTrue else playedFalse

    @tailrec
    def length(direction: (Int, Int), from: Pos, l: Int): Int = {
      val pos = Pos(from.x + direction._1, from.y + direction._2)
      if (played(pos)) length(direction, pos, l + 1)
      else l
    }

    if (played.isEmpty) 0 else {
      val lengths = for {
        d <- Directions.all
        p <- played
      } yield length(d, p, 1)
      lengths.max
    }
  }

  private def allFree = (for {
    i <- 0 until width
    j <- 0 until height
  } yield Pos(i, j)).toSet
}

object Directions {
  val right = (1, 0)
  val down = (0, 1)
  val downRight = (1, 1)
  val downLeft = (-1, 1)
  val all = Seq(right, down, downRight, downLeft)
}

case class Pos(x: Int, y: Int)

