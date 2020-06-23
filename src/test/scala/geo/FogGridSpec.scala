package geo

import geo.Geometry._
import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers, OneInstancePerTest}
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar

@RunWith(classOf[JUnitRunner])
class FogGridSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {
  val height = 1001
  val width = 2 * height
  val scale = 10
  val fg = FogGrid(width, height, scale)

  "a FogGrid" should "initially have old age" in {
    for {
      i <- 0 until width by width / 100
      j <- 0 until height by height / 100
    } fg.age(Pos(i, j)) shouldBe fg.initialAge
  }

  it should "have scale² spots" in {
    fg.spots.size shouldBe scale * scale
  }

  it should "update seen places" in {
    val s = Some(Pos(width / 2, height / 2))
    val u = fg.updated(s, height / 5)
    u.age(Pos(0, 0)) shouldBe fg.initialAge + 1
    u.age(Pos(width / 2, height / 2)) shouldBe 0
    u.age(Pos(width - 1, height - 1)) shouldBe fg.initialAge + 1
  }

  it should "suggest old places" in {
    val fg = FogGrid(width, height, 2)
    val s = List(Pos(0, 0), Pos(0, height / 2), Pos(width / 2, height / 2))
    val u = fg.updated(s, 1)
    u.olderThan(100) should contain theSameElementsAs Seq(Pos(width / 2, 0))
  }

}
