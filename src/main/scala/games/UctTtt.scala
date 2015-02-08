package games

import ai._
import util.Random.nextInt
import scala.annotation.tailrec
import scala.util.Random
import scala.math._

class UctTtt(size: Int, needed: Int) extends TTTGame(size, needed) with UctGame

object UctTtt extends App {
  val game = new UctTtt(size = 3, needed = 3)
  import game._
  var state = TTTState(Board(), X)
  while (state.transitions.nonEmpty) {
    val root = Node(state, TTTSearchPolicy)
    val start = System.nanoTime
    val delay = 1000 * 1000 * 1000 // 1000 ms
    var count = 0
    while (System.nanoTime < start + delay) {
      root.value
      count += 1
    }
    state = root.asInstanceOf[BanditNode].bestTransition.to
    println(s"$count simulations")
    println(state.board + "\n---")
  }
}
