package ai2

import org.scalatest.Outcome
import scala.annotation.tailrec
import scala.util.Random

case class GomokuRules(size: Int, lengthToWin: Int) {
  val initial = GomokuBoard(size, size)

  def outcome(b: GomokuBoard) =
    if (b.maxLength(true) >= lengthToWin) TrueWins
    else if (b.maxLength(false) >= lengthToWin) FalseWins
    else if (b.free.isEmpty) Draw
    else Undecided

  @tailrec
  final def judge(truePl: GomokuBoard => Pos, falsePl: GomokuBoard => Pos, state: GomokuBoard = initial): Outcome = {
    println(state.toText)
    println("---------------")
    outcome(state) match {
      case Undecided =>
        val m = if (state.next) truePl(state) else falsePl(state)
        judge(truePl, falsePl, state.play(m.x, m.y))
      case o => o
    }
  }

  sealed trait Outcome
  case object TrueWins extends Outcome
  case object FalseWins extends Outcome
  case object Undecided extends Outcome
  case object Draw extends Outcome
}

object GomokuDemo extends App {
  def randomPlay(b: GomokuBoard) = {
    val i = Random.nextInt(b.free.size)
    b.free.toVector(i)
  }

  GomokuRules(3, 3).judge(randomPlay, randomPlay)
}
  
