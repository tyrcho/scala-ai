package ai

import org.scalatest.mock.MockitoSugar
import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import games.X
import games.O
import games.AlphaBetaTtt

@RunWith(classOf[JUnitRunner])
class AlphaBetaSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {

  it should "find the next best attack move" in {
    val game = new AlphaBetaTtt(size = 3, needed = 3)
    import game._
    val map = Map(
      (0, 0) -> X,
      (0, 1) -> O,
      (2, 2) -> O,
      (1, 1) -> X)
    var state = TTTState(Board(map), X)
    val move = game.bestMove(state)
    move shouldBe TTTTransition(state, X, 1, 0)
  }

  it should "find the next defense move" in {
    val game = new AlphaBetaTtt(size = 3, needed = 3)
    import game._
    val map = Map(
      (0, 0) -> X,
      (0, 1) -> O,
      (1, 1) -> X)
    var state = TTTState(Board(map), O)
    val move = game.bestMove(state)
    move shouldBe TTTTransition(state, O, 2, 2)
  }
}