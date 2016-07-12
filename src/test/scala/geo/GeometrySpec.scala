package geo

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
import Geometry._

@RunWith(classOf[JUnitRunner])
class GeometrySpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {

  "a Pos" should "implement basic operations" in {
    Pos(3, 5) + Pos(2, 2) shouldBe Pos(5, 7)
    Pos(3, 5) - Pos(2, 2) shouldBe Pos(1, 3)
    Pos(2, 4) * 2 shouldBe Pos(4, 8)
    Pos(2, 4) / 2 shouldBe Pos(1, 2)
  }

}