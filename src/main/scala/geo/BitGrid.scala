package geo

import scala.collection.BitSet
import scala.collection.SortedSet
import ai2.Pos

case class BitGrid(gridData: GridData, masks: Masks) {
  def empty: Boolean = gridData.empty

  def complete: Boolean =
    masks.isComplete(gridData)

  def free = gridData.free

  def used = gridData.used

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
  def apply(size: Int): GridData =
    GridData(size, rows = Vector.fill(size)(0L))

  val fullGridCache = collection.mutable.Map.empty[Int, BitSet]

  def fullGrid(size: Int) = fullGridCache.getOrElseUpdate(size, {
    val all = for {
      r <- 0 until size
      c <- 0 until size
    } yield r * size + c
    BitSet(all: _*)
  })

  def full(size: Int) = GridData(size, Vector.fill(size)((1L << size + 1) - 1))
}

case class GridData(
    size: Int,
    rows: Vector[Long],
    data: BitSet = BitSet.empty) {

  def empty = rows.forall(0.==)

  def +(r: Int, c: Int) = {
    val i = toIndex(r, c)
    copy(
      data = data + i,
      rows = rows.updated(r, rows(r) | 1 << c))
  }

  def -(r: Int, c: Int) = {
    val i = toIndex(r, c)
    copy(
      data = data - i,
      rows = rows.updated(r, rows(r) & ~(1 << c)))
  }

  lazy val free: Iterable[(Int, Int)] =
    for {
      r <- 0 until size
      row = rows(r)
      c <- 0 until size
      if (row & (1 << c)) == 0
    } yield (r, c)
  lazy val used: Iterable[(Int, Int)] =
    for {
      r <- 0 until size
      row = rows(r)
      c <- 0 until size
      if (row & (1 << c)) != 0
    } yield (r, c)

  lazy val usedPos: Set[Pos] = used.map { case (r, c) => Pos(r, c) }.toSet

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

  def subMatrix(r0: Int, c0: Int, subSize: Int): Long = {
    val bits = for {
      x <- 0 until subSize
      r = x + r0
      y <- 0 until subSize
      c = y + c0
      i = x * subSize + y
      bit = (1L << i) & (rows(r) >> c << i)
      //    bit = (1L & hasElt(r, c)) << i
    } yield bit
    bits.sum
  }

  def hasElt(r: Int, c: Int) = (rows(r) << c)
  //    if ((rows(r) & (1L << c)) != 0) 1 else 0

  private def toIndex(r: Int, c: Int) = r * size + c
  private def fromIndex(l: Int) = (l / size, l % size)
}

case class Masks(size: Int, needed: Int) {
  val empty = GridData(size)

  def isComplete(grid: GridData) =
    matrixIndices.exists {
      case (r0, c0) =>
        val gsm = grid.subMatrix(r0, c0, needed)
        matricesCompleted.exists(m => (m & gsm) == m)
    }
  //  masksCompleted.exists(mask =>
  //    (grid.data & mask) == mask)

  val matrixIndices = for {
    r0 <- 0 to size - needed
    c0 <- 0 to size - needed
  } yield (r0, c0)

  def preComputedMatricesRows: Seq[Long] =
    (0 until needed).map(row => (for {
      c <- 0 until needed
      i = row * needed + c
      bit = (1L << i)
    } yield bit).sum)

  def preComputedMatricesCols: Seq[Long] =
    (0 until needed).map(col => (for {
      r <- 0 until needed
      i = r * needed + col
      bit = (1L << i)
    } yield bit).sum)

  def preComputedMatricesDiag1: Long =
    (for {
      r <- 0 until needed
      c = r
      i = r * needed + c
      bit = (1L << i)
    } yield bit).sum

  def preComputedMatricesDiag2: Long =
    (for {
      r <- 0 until needed
      c = needed - 1 - r
      i = r * needed + c
      bit = (1L << i)
    } yield bit).sum

  val matricesCompleted: Set[Long] =
    (preComputedMatricesRows ++ preComputedMatricesCols :+ preComputedMatricesDiag1 :+ preComputedMatricesDiag2).toSet

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