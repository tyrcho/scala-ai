package ai2

case class AlphaBetaGomoku(rules: GomokuRules) {
  def bestMove(board: GomokuBoard, depth: Int = 3): Pos = {
    val moves = board.free.toVector
    val count = moves.size
    val player = board.next
    val valuedMoves = for (m <- moves) yield m -> alphaBetaValue(board.play(m), depth)
    val best = valuedMoves.sortBy(-_._2).head
    best._1
  }

  private def alphaBetaValue(board: GomokuBoard, depth: Int) = {
    val n = negamax(board, depth)
    //    println(s"value for \n$board \n $n \n")
    n
  }

  val MaxValue = 100
  val MinValue = -100

  // gives good score if the player who just played is winning
  def heuristicValue(node: GomokuBoard): Double = {
    val movesPlayed = node.playedFalse.size + node.playedTrue.size
    rules.outcome(node) match {
      case TrueWins  => if (!node.next) MaxValue - movesPlayed else MinValue + movesPlayed
      case FalseWins => if (node.next) MaxValue - movesPlayed else MinValue + movesPlayed
      case Draw      => 0
      case Undecided => -node.maxLength(node.next) + node.maxLength(!node.next)
    }
  }

  // gives good score if the player who just played is winning
  private def negamax(
    node: GomokuBoard,
    depth: Int,
    alphaIni: Double = MinValue,
    beta: Double = MaxValue): Double = {
    val transitions = node.free
    if (depth == 0 || transitions.isEmpty || rules.outcome(node) != Undecided) {
      val h = heuristicValue(node)
      println(s"heuristic=$h for \n$node")
      h
    } else {
      var alpha = alphaIni
      val it = transitions.iterator
      var break = false
      while (it.hasNext && !break) {
        val board = node.play(it.next())
        val score = -negamax(board, depth - 1, -beta, -alpha)
        println(s"score $score for \n$board")
        if (score >= beta) {
          alpha = beta
          break = true
        } else if (score > alpha) {
          alpha = score
        }
      }
      println(s"returning $alpha for \n$node")
      alpha
    }
  }
}