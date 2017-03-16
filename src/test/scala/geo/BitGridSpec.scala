package geo

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BitGridSpec extends FlatSpec with Matchers {

  "a BitGrid" should "initially be empty" in {
    val bg = BitGrid(3, 3)
    bg.complete shouldBe false
    bg.empty shouldBe true
  }

  it should "detect a full row" in {
    val bg = BitGrid(3, 3) + (0, 0) + (0, 1) + (0, 2)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

  it should "detect a full col" in {
    val bg = BitGrid(3, 3) + (0, 1) + (1, 1) + (2, 1)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

  it should "detect a diag1" in {
    val bg = BitGrid(3, 3) + (0, 0) + (1, 1) + (2, 2)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

  it should "detect a diag2" in {
    val bg = BitGrid(3, 3) + (0, 2) + (1, 1) + (2, 0)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

}