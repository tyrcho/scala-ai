package ai2

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AlphaBetaGomokuSpec extends FlatSpec with Matchers {
  val rules = GomokuRules(3, 3)
  val ab = AlphaBetaGomoku(rules)

//  "AlphaBeta" should "find kill move" in {
//    val board = rules.initial
//      .play(1, 1).play(0, 0)
//      .play(0, 1).play(2, 2)
//    for (depth <- 1 to 10)
//      ab.bestMove(board, depth) shouldBe Pos(2, 1)
//  }

//  "ab" should "find avoid kill move" in {
//    val board = rules.initial
//      .play(1, 1).play(0, 0)
//      .play(0, 1)
//    for (depth <- 2 to 10)
//      ab.bestMove(board, depth) shouldBe Pos(2, 1)
//  }

  it should "give good score to won board" in {
    val board = rules.initial
      .play(1, 1).play(0, 0)
      .play(0, 1).play(2, 2)
      .play(2, 1)
    ab.heuristicValue(board).toInt should be > 50
  }

}
