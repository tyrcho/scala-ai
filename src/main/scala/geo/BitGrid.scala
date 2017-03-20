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

  val fullGridCache = collection.mutable.Map.empty[Int, GridData]

  def full(size: Int) = fullGridCache.getOrElseUpdate(size,
    GridData(size, Vector.fill(size)((1L << size + 1) - 1)))
}

case class GridData(
    size: Int,
    rows: Vector[Long]) {

  def empty = rows.forall(0.==)

  def +(r: Int, c: Int) = {
    val i = toIndex(r, c)
    copy(rows = rows.updated(r, rows(r) | 1 << c))
  }

  def -(r: Int, c: Int) = {
    val i = toIndex(r, c)
    copy(rows = rows.updated(r, rows(r) & ~(1 << c)))
  }

  lazy val free: Iterable[(Int, Int)] = {
    var r = 0
    var buffer = collection.mutable.Buffer[(Int, Int)]()
    while (r < size) {
      var row = rows(r)
      var c = 0
      while (c < size) {
        if ((row & (1 << c)) == 0)
          buffer += ((r, c))
        c += 1
      }
      r += 1
    }
    buffer
  }

  lazy val used: Iterable[(Int, Int)] = {
    var r = 0
    var buffer = collection.mutable.Buffer[(Int, Int)]()
    while (r < size) {
      var row = rows(r)
      var c = 0
      while (c < size) {
        if ((row & (1 << c)) != 0)
          buffer += ((r, c))
        c += 1
      }
      r += 1
    }
    buffer
  }

  lazy val usedPos: Set[Pos] = {
    var r = 0
    var buffer = Set.empty[Pos]
    while (r < size) {
      var row = rows(r)
      var c = 0
      while (c < size) {
        if ((row & (1 << c)) != 0)
          buffer += Pos(r, c)
        c += 1
      }
      r += 1
    }
    buffer
  }

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
    var sum = 0L
    var x = 0
    while (x < subSize) {
      val r = x + r0
      var y = 0
      while (y < subSize) {
        val c = y + c0
        val i = x * subSize + y
        val bit = (1L << i) & (rows(r) >> c << i)
        sum += bit
        y += 1
      }
      x += 1
    }
    sum
  }

  private def toIndex(r: Int, c: Int) = r * size + c
  private def fromIndex(l: Int) = (l / size, l % size)
}

object Masks {
  val matricesCache = collection.mutable.Map.empty[Int, Seq[Long]]

  def matricesCompleted(needed: Int): Seq[Long] = matricesCache.getOrElseUpdate(needed, {
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

    preComputedMatricesRows ++ preComputedMatricesCols :+ preComputedMatricesDiag1 :+ preComputedMatricesDiag2
  })

}

case class Masks(size: Int, needed: Int) {
  val empty = GridData(size)

  def isComplete(grid: GridData) = {
    var found = false
    var r0 = 0
    while (r0 <= size - needed && !found) {
      var c0 = 0
      while (c0 <= size - needed && !found) {
        val gsm = grid.subMatrix(r0, c0, needed)
        var i = 0
        while (i < mcSize && !found) {
          val matrix = matricesCompleted(i)
          found ||= (matrix & gsm) == matrix
          i += 1
        }
        c0 += 1
      }
      r0 += 1
    }
    found
  }

  val matricesCompleted: Seq[Long] = Masks.matricesCompleted(needed)
  val mcSize = matricesCompleted.size
}

object BitGrid {

  def apply(size: Int, needed: Int): BitGrid =
    BitGrid(GridData(size), Masks(size, needed))
}