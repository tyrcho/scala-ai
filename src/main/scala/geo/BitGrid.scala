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

  def addRow(r: Int) = copy(gridData = gridData.addRow(r))
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

  def addCol(c: Int) =
    (0 until size).foldLeft(this) {
      case (bg, r) => bg + (r, c)
    }

  def addRow(r: Int) =
    (0 until size).foldLeft(this) {
      case (bg, c) => bg + (r, c)
    }

  private def toIndex(r: Int, c: Int) = r * size + c
  private def fromIndex(l: Int) = (l / size, l % size)
}

case class Masks(size: Int, needed: Int) {
  val empty = GridData(size)

  val masksCompleted: Seq[BitSet] = {

    def maskRow(r: Int) = empty.addRow(r).data

    def maskDiag1 = {
      (0 until size).foldLeft(empty) {
        case (bg, i) => bg + (i, i)
      }.data
    }

    def maskDiag2 = {
      (0 until size).foldLeft(empty) {
        case (bg, i) => bg + (size - 1 - i, i)
      }.data
    }

    def maskCol(c: Int) = empty.addCol(c).data

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