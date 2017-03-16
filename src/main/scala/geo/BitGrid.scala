package geo

import scala.collection.BitSet
import scala.collection.SortedSet

case class BitGrid(gridData: GridData, masks: Masks) {
  def empty: Boolean = gridData.data.isEmpty

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
  def addCol(c: Int, minRow: Int, maxRow: Int) = copy(gridData = gridData.addCol(c, minRow, maxRow))

  def addRow(r: Int) = copy(gridData = gridData.addRow(r))
  def addRow(r: Int, minCol: Int, maxCol: Int) = copy(gridData = gridData.addRow(r, minCol, maxCol))

  def addDiag1(r: Int, c: Int, length: Int) = copy(gridData = gridData.addDiag1(r, c, length))
  def addDiag2(r: Int, c: Int, length: Int) = copy(gridData = gridData.addDiag2(r, c, length))
}

object GridData {
  val fullGridCache = collection.mutable.Map.empty[Int, GridData]

  def fullGrid(size: Int) = fullGridCache.getOrElseUpdate(size, {
    val all = for {
      r <- 0 until size
      c <- 0 until size
    } yield r * size + c
    GridData(size, BitSet(all: _*))
  })
}

case class GridData(size: Int, data: BitSet = BitSet.empty) {
  def +(r: Int, c: Int) = {
    copy(data = data + toIndex(r, c))
  }

  def -(r: Int, c: Int) = {
    copy(data = data - toIndex(r, c))
  }

  def free: SortedSet[(Int, Int)] = GridData.fullGrid(size).data.&~(data).map(fromIndex)

  def addCol(c: Int, minRow: Int = 0, maxRow: Int = size) =
    (minRow until maxRow).foldLeft(this) {
      case (bg, r) => bg + (r, c)
    }

  def addRow(r: Int, minCol: Int = 0, maxCol: Int = size) =
    (minCol until maxCol).foldLeft(this) {
      case (bg, c) => bg + (r, c)
    }

  def addDiag1(r: Int, c: Int, size: Int = size) =
    (0 until size).foldLeft(this) {
      case (bg, i) => bg + (r + i, c + i)
    }

  def addDiag2(r: Int, c: Int, size: Int = size) =
    (0 until size).foldLeft(this) {
      case (bg, i) => bg + (r + i, c - i)
    }

  private def toIndex(r: Int, c: Int) = r * size + c
  private def fromIndex(l: Int) = (l / size, l % size)
}

case class Masks(size: Int, needed: Int) {
  val empty = GridData(size)

  val masksCompleted: Seq[BitSet] = {

    def maskCol(c: Int, rMin: Int, rMax: Int) = empty.addCol(c, rMin, rMax).data
    def maskRow(r: Int, cMin: Int, cMax: Int) = empty.addRow(r, cMin, cMax).data

    def maskDiag1(r: Int, c: Int) = empty.addDiag1(r, c, needed).data

    def maskDiag2(r: Int, c: Int) = empty.addDiag2(r, c, needed).data

    val hv = for {
      pos <- 0 until size
      pos2Min <- 0 to size - needed
      pos2Max = pos2Min + needed
      mask <- Seq(maskRow(pos, pos2Min, pos2Max), maskCol(pos, pos2Min, pos2Max))
    } yield mask

    val diags = for {
      r <- 0 to size - needed
      c <- 0 to size - needed
      mask <- Seq(maskDiag1(r, c), maskDiag2(r, size - 1 - c))
    } yield mask

    hv ++ diags
  }

}

object BitGrid {

  def apply(size: Int, needed: Int): BitGrid =
    BitGrid(GridData(size), Masks(size, needed))
}