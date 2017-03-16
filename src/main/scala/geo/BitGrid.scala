package geo

case class BitGrid(gridData: GridData, masks: Masks) {
  def empty: Boolean = gridData.data == 0

  def complete: Boolean =
    masks.masksCompleted.exists(mask =>
      (gridData.data & mask) == mask)

  def free = gridData.free

  def +(r: Int, c: Int) = {
    copy(gridData = gridData + (r, c))
  }

  def -(r: Int, c: Int) = {
    copy(gridData = gridData - (r, c))
  }

  def addCol(c: Int) = copy(gridData = gridData.addCol(c))

  def addRow(r: Int) = copy(gridData = gridData.addRow(r))
}

case class GridData(size: Int, data: Long = 0) {
  def +(r: Int, c: Int) = {
    copy(data = data | toMask(r, c))
  }

  def -(r: Int, c: Int) = {
    copy(data = data & ~toMask(r, c))
  }

  def free = for {
    r <- 0 until size
    c <- 0 until size
    if (data & toMask(r, c)) == 0
  } yield (r, c)

  def addCol(c: Int) =
    (0 until size).foldLeft(this) {
      case (bg, r) => bg + (r, c)
    }

  def addRow(r: Int) =
    (0 until size).foldLeft(this) {
      case (bg, c) => bg + (r, c)
    }

  private def toIndex(x: Int, y: Int) = x * size + y
  private def toMask(x: Int, y: Int) = 1 << toIndex(x, y)
}

case class Masks(size: Int, needed: Int) {
  val empty = GridData(size)

  val masksCompleted: Seq[Long] = {

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

object BitGrid {

  def apply(size: Int, needed: Int): BitGrid =
    BitGrid(GridData(size), Masks(size, needed))
}