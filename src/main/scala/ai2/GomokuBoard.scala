package ai2

import scala.annotation.tailrec
import geo.GridData
import scala.collection.immutable.BitSet
import geo.BitGrid
import geo.Masks

object GomokuBoard {
  def apply(size: Int): GomokuBoard =
    GomokuBoard(
      size,
      dataTrue = GridData(size),
      dataFalse = GridData(size),
      dataFree = GridData(size, rows = Vector.fill(size)((1 << (size + 1)) - 1)), next = false)
}

case class GomokuBoard(
    size: Int,
    dataTrue: GridData,
    dataFalse: GridData,
    dataFree: GridData,
    next: Boolean = false) {

  def play(x: Int, y: Int): GomokuBoard = play(Pos(x, y))

  def play(p: Pos): GomokuBoard =
    if (next)
      copy(
        next = false,
        dataFree = dataFree - (p.x, p.y),
        dataTrue = dataTrue + (p.x, p.y))
    else
      copy(
        next = true,
        dataFree = dataFree - (p.x, p.y),
        dataFalse = dataFalse + (p.x, p.y))

  lazy val playedFalse = dataFalse.usedPos
  lazy val playedTrue = dataTrue.usedPos

  lazy val free = dataFree.usedPos

  override def toString = toText

  def toText: String = Seq.tabulate(size) { y =>
    Seq.tabulate(size) { x =>
      if (playedFalse.toSet(Pos(x, y))) 'F'
      else if (playedTrue.toSet(Pos(x, y))) 'T'
      else ' '
    }.mkString
  }.mkString("\n") + s"\n${next.toString.toUpperCase.head} to play"

  def hasWon(player: Boolean, needed: Int) = {
    val data = if (player) dataTrue else dataFalse
    BitGrid(data, Masks(size, needed)).complete
  }

  def maxLength(player: Boolean) = {
    val played = if (player) playedTrue else playedFalse

    @tailrec
    def length(direction: (Int, Int), from: Pos, l: Int): Int = {
      val pos = Pos(from.x + direction._1, from.y + direction._2)
      if (played.toSet(pos)) length(direction, pos, l + 1)
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
}

object Directions {
  val right = (1, 0)
  val down = (0, 1)
  val downRight = (1, 1)
  val downLeft = (-1, 1)
  val all = Seq(right, down, downRight, downLeft)
}

case class Pos(x: Int, y: Int)

