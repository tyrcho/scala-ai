package geo

case class BitGrid(size: Int, needed: Int, data: Long = 0, masks: Seq[Long] = Seq.empty) {
  def empty: Boolean = data == 0
  def complete: Boolean =
    masks.exists(mask =>
      (data & mask) == mask)

  def +(x: Int, y: Int): geo.BitGrid = {
    val i = toIndex(x, y)

    copy(data = data | 1 << i)
  }

  private def toIndex(x: Int, y: Int) = x * size + y
}

object BitGrid {

  def apply(size: Int, needed: Int): BitGrid =
    BitGrid(size, needed, 0, masks(size, needed))

  def masks(size: Int, needed: Int): Seq[Long] = {
    val empty = BitGrid(size, needed, 0, Seq.empty)

    def maskRow(r: Int): Long = {
      (0 until size).foldLeft(empty) {
        case (bg, c) => bg + (r, c)
      }.data
    }

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

    def maskCol(c: Int): Long = {
      (0 until size).foldLeft(empty) {
        case (bg, r) => bg + (r, c)
      }.data
    }

    val hv = for {
      i <- 0 until size
      mask <- Seq(maskRow(i), maskCol(i))
    } yield mask

    hv :+ maskDiag1 :+ maskDiag2
  }
}