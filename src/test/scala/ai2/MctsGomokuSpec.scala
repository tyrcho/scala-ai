package ai2

import org.scalatest.mock.MockitoSugar
import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import math._
import org.scalatest.matchers.ShouldMatchers._
import org.mockito.Mockito._
import org.mockito.Matchers._

@RunWith(classOf[JUnitRunner])
class MctsGomokuSpec extends FlatSpec with Matchers with MockitoSugar {
  val rules = GomokuRules(3, 3)
  val board = rules.initial

  val mc = MctsGomoku(rules, _ => TrueWins)

  "mcts" should "initially have 0 results" in {
    mc.results shouldBe Results(played = 0, trueWins = 0)
  }

  it should "have 0 nodes initially" in {
    mc.nodes should have size (0)
  }

  it should "expand the root node to one child" in {
    val nodes = mc.expand.nodes

    val move = board.free.head
    val expectedBoard = board.play(move)
    nodes should have size (1)
    nodes(move).board shouldBe expectedBoard
  }

  it should "expand the root node to a second child" in {
    val (move, child) = mc.expand.nodes.head
    val c1 = child.recordSimulation

    val mc2 = mc.copy(nodes = Map(move -> c1)).expand

    mc2.nodes should have size (2)
  }

  it should "step up to 9 children" in {
    mc.step.nodes should have size (1)
    mc.step.step.nodes should have size (2)
    mc.step.step.step.nodes should have size (3)
    mc.step(9).nodes should have size (9)
    val step10 = mc.step(10)
    step10.nodes should have size (9)
    step10.nodes.values.count(_.nodes.size == 1) shouldBe 1
  }

  it should "simulate the selected node and record the result" in {
    val s = mc.recordSimulation
    s.results.played shouldBe 1
    s.results.score shouldBe 1
  }

  it should "backtrack the results to root node" in {
    val sim = mock[GomokuBoard => Outcome]
    when(sim.apply(any())).thenReturn(TrueWins, TrueWins, FalseWins)
    val root = MctsGomoku(rules, sim)
    val c1 = root.makeChild(Pos(0, 0)).recordSimulation
    val c2 = root.makeChild(Pos(1, 0)).recordSimulation
    val c3 = root.makeChild(Pos(1, 1)).recordSimulation

    val mcts = root.copy(nodes = Map(Pos(0, 0) -> c1))
    mcts.results.played shouldBe 1
    mcts.results.score shouldBe 1F

    val mcts2 = root.copy(nodes = Map(
      Pos(0, 0) -> c1,
      Pos(1, 0) -> c2,
      Pos(1, 1) -> c3))
    mcts2.results.played shouldBe 3
    mcts2.results.score shouldBe 2 / 3F +- 0.00001F
  }

}