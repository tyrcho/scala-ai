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

  it should "detect a full row with extra" in {
    val bg = BitGrid(3, 3) + (0, 0) + (0, 1) + (0, 2) + (1, 0)
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

  it should "detect incomplete" in {
    val bg = BitGrid(3, 3) + (0, 1) + (1, 1) + (2, 0)
    bg.empty shouldBe false
    bg.complete shouldBe false
  }

  it should "add a full col" in {
    val empty = BitGrid(3, 3)
    val bg = empty.addCol(0)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

  it should "detect a full row in a large grid" in {
    val empty = BitGrid(19, 19)
    val bg = empty.addCol(11)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

  it should "detect a full row - 1 in a large grid" in {
    val empty = BitGrid(19, 19)
    val bg = empty.addCol(11) - (5, 11)
    bg.empty shouldBe false
    bg.complete shouldBe false
  }

  it should "list free cells" in {
    val empty = BitGrid(3, 3)
    empty.free.size shouldBe 9
  }

  it should "list free cells (medium)" in {
    val size = 5
    val empty = BitGrid(size, size)
    val bg = empty.addCol(size / 2) - (size / 4, size / 2)
    bg.free.size shouldBe (size * (size - 1) + 1)
  }

  it should "list free cells (large)" in {
    val size = 19
    val empty = BitGrid(size, size)
    val bg = empty.addCol(size / 2) - (size / 4, size / 2)
    bg.free.size shouldBe (size * (size - 1) + 1)
  }

  it should "detect a partial row " in {
    val empty = BitGrid(19, 5)
    val bg = empty.addCol(11, 5, 10)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

  it should "detect a partial col" in {
    val empty = BitGrid(19, 6)
    val bg = empty.addRow(17, 11, 17)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

  it should "detect a partial diag1" in {
    val empty = BitGrid(19, 6)
    val bg = empty.addDiag1(3, 3, 6)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

  it should "detect a partial diag2" in {
    val empty = BitGrid(19, 6)
    val bg = empty.addDiag2(12, 18, 6)
    bg.empty shouldBe false
    bg.complete shouldBe true
  }

}