package ai

import scala.collection.mutable.ListBuffer

object BackPack {
  def best[T](items: Seq[T], maxWeight: Int, weightFun: T => Int, valueFun: T => Int): Seq[T] = {
    //https://openclassrooms.com/courses/introduction-a-la-programmation-dynamique

    def nextLine(line: Seq[Int], item: T): Seq[Int] = {
      val itemWeight = weightFun(item)
      val itemValue = valueFun(item)
      Seq.tabulate(maxWeight) { c =>
        val w = c + 1
        val tooBig = itemWeight > w
        val optimalWithoutItem = line(c)
        if (tooBig) optimalWithoutItem
        else if (w == itemWeight) optimalWithoutItem max itemValue
        else {
          val optimalWithItem = itemValue + line(c - itemWeight)
          optimalWithoutItem max optimalWithItem
        }
      }
    }

    val line0 = Seq.tabulate(maxWeight) { c =>
      val itemWeight = weightFun(items(0))
      val itemValue = valueFun(items(0))
      val tooBig = itemWeight > c + 1
      if (tooBig) 0 else itemValue
    }

    val matrix = items.tail.scanLeft(line0) {
      case (line, item) => nextLine(line, item)
    }

    def selectedItem(line: Int = items.length - 1, column: Int = maxWeight - 1, acc: Seq[T] = Seq.empty[T]): Seq[T] = {
      if (line == 0) {
        if (matrix(line)(column) == 0) {
          acc
        } else {
          items(0) +: acc
        }
      } else {
        if (matrix(line)(column) == matrix(line - 1)(column)) {
          selectedItem(line - 1, column, acc)
        } else {
          selectedItem(line - 1, column - weightFun(items(line)), items(line) +: acc)
        }
      }
    }

    selectedItem()
  }
}