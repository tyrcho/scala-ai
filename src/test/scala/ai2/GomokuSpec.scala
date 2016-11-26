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

@RunWith(classOf[JUnitRunner])
class GomokuSpec extends FlatSpec with Matchers {
  val initial = GomokuBoard(5, 6)

  "Gomoku" should "be initially empty" in {
    initial.playedTrue shouldBe Set()
    initial.playedFalse shouldBe Set()
  }

  it should "keep played moves" in {
    val state = initial.play(1, 1).play(1, 2)
    state.playedTrue shouldBe Set(Pos(1, 2))
    state.playedFalse shouldBe Set(Pos(1, 1))
  }

  it should "list free cells" in {
    val all = (for {
      i <- 0 to 4
      j <- 0 to 5
    } yield Pos(i, j)).toSet

    initial.free shouldBe all

    val state = initial.play(1, 1).play(1, 2)
    state.free shouldBe (all - Pos(1, 1) - Pos(1, 2))
  }

  it should "give max line length" in {
    val state = initial.play(1, 1).play(3, 3)
      .play(2, 2)
    state.maxLength(false) shouldBe 2
    state.maxLength(true) shouldBe 1
  }
}