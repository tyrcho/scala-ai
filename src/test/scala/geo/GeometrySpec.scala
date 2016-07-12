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
import math._

@RunWith(classOf[JUnitRunner])
class GeometrySpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {

  "a Pos" should "implement basic operations" in {
    Pos(3, 5) + Pos(2, 2) shouldBe Pos(5, 7)
    Pos(3, 5) - Pos(2, 2) shouldBe Pos(1, 3)
    Pos(2, 4) * 2 shouldBe Pos(4, 8)
    Pos(2, 4) / 2 shouldBe Pos(1, 2)

    Pos(5, 10) dist2 Pos(10, 5) shouldBe 50
    Pos(5, 10) dist Pos(10, 5) shouldBe sqrt(50)

    Pos(3, 4).norm shouldBe 5
  }

  it should "find the closest point to a line" in {
    Pos(0, 0).closest(Pos(0, 2), Pos(2, 0)) shouldBe Pos(1, 1)
    Pos(0, 0).closest(Pos(0, 2), Pos(0, 0)) shouldBe Pos(0, 0)
    Pos(0, 1).closest(Pos(1, 2), Pos(2, 1)) shouldBe Pos(1, 2)
  }

  "an entity" should "manage angles" in {
    val origin = Entity(pos = Pos(0, 0), angle = EAST)
    origin.angleTo(Pos(5, 0)) shouldBe EAST
    origin.angleTo(Pos(0, 5)) shouldBe SOUTH
    origin.angleTo(Pos(-5, 0)) shouldBe WEST
    origin.angleTo(Pos(0, -5)) shouldBe NORTH

    origin.deltaAngleToFace(Pos(5, 0)) shouldBe 0
    origin.deltaAngleToFace(Pos(0, 5)) shouldBe 90
    origin.deltaAngleToFace(Pos(-5, 0)) shouldBe 180
    origin.deltaAngleToFace(Pos(0, -5)) shouldBe -90

    origin.rotateTo(Pos(5, 0)).angle shouldBe 0
    origin.rotateTo(Pos(0, 5)).angle shouldBe MAX_ROTATION
    origin.rotateTo(Pos(-5, 0)).angle shouldBe MAX_ROTATION
    origin.rotateTo(Pos(0, -5)).angle shouldBe 360 - MAX_ROTATION
  }

}