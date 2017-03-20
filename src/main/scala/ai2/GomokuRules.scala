package ai2

import scala.annotation.tailrec
import scala.util.Random

case class GomokuRules(size: Int, lengthToWin: Int) {
  val initial: ai2.GomokuBoard = GomokuBoard(size)

  def outcome(b: GomokuBoard) =
    if (b.hasWon(true, lengthToWin)) TrueWins
    else if (b.hasWon(false, lengthToWin)) FalseWins
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

}

sealed trait Outcome
case object TrueWins extends Outcome
case object FalseWins extends Outcome
case object Undecided extends Outcome
case object Draw extends Outcome

object GomokuDemo extends App {
  def randomPlay(b: GomokuBoard): ai2.Pos = {
    val i = Random.nextInt(b.free.size)
    b.free.toVector(i)
  }

  val rules: ai2.GomokuRules = GomokuRules(3, 3)
  val ab: ai2.AlphaBetaGomoku = AlphaBetaGomoku(rules)
  def alphaBetaPlay(b: GomokuBoard): ai2.Pos = ab.bestMove(b, 9)

  rules.judge(alphaBetaPlay, randomPlay)
}

