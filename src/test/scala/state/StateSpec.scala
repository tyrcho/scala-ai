package state

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers, OneInstancePerTest}
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar

@RunWith(classOf[JUnitRunner])
class StateSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {
  type IntState = State[Int, Int]
  val incr: IntState = State { i => (i + 1, i + 1) }

  "a State " should "implement map/flatMap" in {
    val incr2 = for {
      a <- incr
      b <- incr
    } yield (a, b)

    incr2.eval(5) shouldBe (6, 7)
  }

  it should "have a reduce method to group computations" in {
    val operations = Seq.fill(10)(incr)

    State.reduce(operations).eval(5) shouldBe 15
  }

  it should "have a sequence method to keep intermediate results" in {
    val operations = Seq.fill(3)(incr)
    State.sequence(operations).eval(5) shouldBe Seq(6, 7, 8)
  }
}
