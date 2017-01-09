package ai

import scala.collection.mutable.ListBuffer

object BackPack {
  def best[T](items: Seq[T], maxWeight: Int, weightFun: T => Int, valueFun: T => Int): Seq[T] = {
    //https://openclassrooms.com/courses/introduction-a-la-programmation-dynamique
    var matrix = Array.fill(items.size, maxWeight)(0)
    var j = 0
    while (j < maxWeight) {
      matrix(0)(j) = if (weightFun(items(0)) > j) 0 else valueFun(items(0))
      j += 1
    }
    var i = 1
    while (i < items.size) {
      j = 0
      while (j < maxWeight) {
        var weight = weightFun(items(i))
        matrix(i)(j) =
          if (weight > j) matrix(i - 1)(j)
          else math.max(matrix(i - 1)(j), matrix(i - 1)(j - weight) + valueFun(items(i)))
        j += 1
      }
      i += 1
    }
    i-=1
    j-=1
    val selection = ListBuffer.empty[T]
    while (matrix(i)(j) == matrix(i - 1)(j)) i -= 1
    while (j > 0) {
      while (i > 0 && matrix(i)(j) == matrix(i - 1)(j)) i -= 1
      j -= weightFun(items(i))
      if (j > 0) selection += items(i)
      i -= 1
    }
    selection.toSeq
  }
}