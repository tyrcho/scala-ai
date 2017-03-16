package geo

case class BitGrid(size: Int, needed: Int, data: Long = 0, masks: Seq[Long] = Seq.empty) {
  def empty: Boolean = data == 0
  def complete: Boolean =
    masks.exists(mask =>
      (data & mask) == mask)

  def +(r: Int, c: Int): geo.BitGrid = {
    copy(data = data | toMask(r, c))
  }

  def -(r: Int, c: Int): geo.BitGrid = {
    copy(data = data & ~toMask(r, c))
  }

  private def toIndex(x: Int, y: Int) = x * size + y
  private def toMask(x: Int, y: Int) = 1 << toIndex(x, y)

  def addCol(c: Int) =
    (0 until size).foldLeft(this) {
      case (bg, r) => bg + (r, c)
    }

  def addRow(r: Int) =
    (0 until size).foldLeft(this) {
      case (bg, c) => bg + (r, c)
    }
}

object BitGrid {

  def apply(size: Int, needed: Int): BitGrid =
    BitGrid(size, needed, 0, masks(size, needed))

  def masks(size: Int, needed: Int): Seq[Long] = {
    val empty = BitGrid(size, needed, 0, Seq.empty)

    def maskRow(r: Int): Long = empty.addRow(r).data

    def maskDiag1: Long = {
      (0 until size).foldLeft(empty) {
        case (bg, i) => bg + (i, i)
      }.data
    }

    def maskDiag2: Long = {
      (0 until size).foldLeft(empty) {
        case (bg, i) => bg + (size - 1 - i, i)
      }.data
    }

    def maskCol(c: Int): Long = empty.addCol(c).data

    val hv = for {
      i <- 0 until size
      mask <- Seq(maskRow(i), maskCol(i))
    } yield mask

    hv :+ maskDiag1 :+ maskDiag2
  }
}