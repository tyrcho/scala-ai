package geo

import org.scalatest.mock.MockitoSugar
import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
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

  it should "move towards a direction" in {
    Pos(0, 0).moveTo(Pos(0, 2), 2) shouldBe Pos(0, 2)
    val Pos(x, y) = Pos(0, 0).moveTo(Pos(2, 2), sqrt(2))
    x shouldBe 1
    y shouldBe 1
  }

  it should "not go pass tgt" in {
    Pos(0, 0).moveTo(Pos(0, 2), 5) shouldBe Pos(0, 2)
    Pos(0, 0).moveTo(Pos(2, 2), 5) shouldBe Pos(2, 2)
  }

  it should "rotate" in {
    def check(p: Pos, center: Pos, angle: Double, expected: Pos) = {
      val rotated = p.rotate(angle, center)
      rotated.x shouldBe expected.x +- 0.001
      rotated.y shouldBe expected.y +- 0.001
    }
    check(Pos(10, 10), Pos(0, 10), -Math.PI / 2, Pos(0, 20))
    check(Pos(10, 10), Pos(0, 10), Math.PI / 2, Pos(0, 0))
  }

  val origin = Entity(pos = Pos(0, 0), angle = EAST)
  val closeEast = Pos(5, 0)
  val closeSouth = Pos(0, 5)
  val closeWest = Pos(-5, 0)
  val closeNorth = Pos(0, -5)

  "an entity" should "manage angles" in {
    origin.angleTo(closeEast) shouldBe EAST
    origin.angleTo(closeSouth) shouldBe SOUTH
    origin.angleTo(closeWest) shouldBe WEST
    origin.angleTo(closeNorth) shouldBe NORTH

    origin.deltaAngleToFace(closeEast) shouldBe 0
    origin.deltaAngleToFace(closeSouth) shouldBe 90
    origin.deltaAngleToFace(closeWest) shouldBe 180
    origin.deltaAngleToFace(closeNorth) shouldBe -90

    origin.rotateTo(closeEast).angle shouldBe 0
    origin.rotateTo(closeSouth).angle shouldBe MAX_ROTATION
    origin.rotateTo(closeWest).angle shouldBe MAX_ROTATION
    origin.rotateTo(closeNorth).angle shouldBe 360 - MAX_ROTATION
  }

  it should "accelerate" in {
    origin.accel(100).speed shouldBe Pos(100, 0)
    val southSpeed = origin.copy(angle = SOUTH).accel(100).speed
    southSpeed.x shouldBe 0.0 +- 0.0001
    southSpeed.y shouldBe 100.0 +- 0.0001
    origin.accel(300).speed shouldBe Pos(MAX_THRUST, 0)
  }

  it should "play a move" in {
    val t1 = origin.move(closeSouth, 100)
    t1.speed.norm shouldBe 100 * FRICTION +- 1
    t1.angle shouldBe MAX_ROTATION
    t1.pos shouldBe Pos(95, 30)
  }

}