package ai2

import scala.annotation.tailrec
import scala.util.Random

object MctsGomokuDemo extends App {
  val rules = GomokuRules(3, 3)
  def mctsPlayer(board: GomokuBoard): Pos = {
    val mcts = MctsGomoku(rules).copy(board = board).step(10000)
    //    mcts.debug()
    mcts.nodes.maxBy(_._2.results.played)._1
  }
  def randomPlayer(board: GomokuBoard): Pos = {
    val options = board.free.toSeq
    options(Random.nextInt(options.size))
  }
  rules.judge(randomPlayer, mctsPlayer, rules.initial.copy(next = true))
  //  val mcts = MctsGomoku(GomokuRules(3, 3))
  //  val s = mcts.step(2000)
  //  s.debug()
}

case class MctsGomoku(
    rules: GomokuRules,
    selectNodeHeuristic: MctsGomoku => Pos => Double,
    simulation: GomokuBoard => Outcome,
    board: GomokuBoard,
    resultsData: Option[Results] = None,
    nodes: Map[Pos, MctsGomoku] = Map.empty) {

  def debug() = {
    for {
      (p, node) <- nodes
    } println(p, node.results)
  }

  @tailrec
  final def step(count: Int): MctsGomoku =
    if (count == 0) this
    else {
      step.step(count - 1)
    }

  def step: MctsGomoku =
    if (board.free.isEmpty) this
    else {
      val move = board.free.maxBy(selectNodeHeuristic(this))
      val node = nodes.get(move) match {
        case Some(mcts) =>
          mcts.step
        case None =>
          makeChild(move).recordSimulation
      }
      copy(nodes = nodes + (move -> node))
    }

  def expand: MctsGomoku = {
    val move = board.free.maxBy(selectNodeHeuristic(this))
    addChild(move)
  }

  lazy val results: Results =
    nodes.values.foldLeft(resultsData.getOrElse(Results())) {
      case (res, node) => res + node.results
    }

  def addChild(move: Pos): MctsGomoku = {
    val child = makeChild(move)
    copy(nodes = nodes + (move -> makeChild(move)))
  }

  def makeChild(move: Pos): MctsGomoku = {
    val b = board.play(move)
    copy(board = b, resultsData = None, nodes = Map.empty)
  }

  def recordSimulation: MctsGomoku = {
    val o = simulation(board)
    copy(resultsData = Some(results + o))
  }

}

object MctsGomoku {
  def selectMove(b: GomokuBoard): Pos = b.free.head

  def expandOneNode(mcts: MctsGomoku) =
    if (mcts.nodes.isEmpty) {
      val move = selectMove(mcts.board)
      mcts.addChild(move)
    } else {
      mcts.board.free
    }

  def outcome(rules: GomokuRules)(b: GomokuBoard) =
    if (b.maxLength(true) >= rules.lengthToWin) TrueWins
    else if (b.maxLength(false) >= rules.lengthToWin) FalseWins
    else if (b.free.isEmpty) Draw
    else Undecided

  @tailrec
  final def randomPlay(rules: GomokuRules)(state: GomokuBoard): Outcome = {
    outcome(rules)(state) match {
      case Undecided =>
        val options = state.free.toSeq
        val m = options(Random.nextInt(options.size))
        randomPlay(rules)(state.play(m.x, m.y))
      case o => o
    }
  }

  // highest is more likely to be the best move for player to play
  def uctHeuristic(mcts: MctsGomoku)(move: Pos): Double = {
    val trueToPlay = mcts.board.next
    val total = mcts.results.played
    mcts.nodes.get(move).map(_.results).map {
      case Results(played, trueWins) =>
        val wins = if (trueToPlay) trueWins else played - trueWins
        val uct = wins / played + math.sqrt(2 * math.log(total) / played)
        uct
    }.getOrElse(Double.MaxValue)
  }

  def apply(rules: GomokuRules): MctsGomoku =
    apply(rules, randomPlay(rules))

  def apply(rules: GomokuRules, simulation: GomokuBoard => Outcome): MctsGomoku =
    MctsGomoku(rules, uctHeuristic, simulation, rules.initial)

}

case class Results(played: Int = 0, trueWins: Float = 0) {
  def +(o: Outcome) = o match {
    case TrueWins         => copy(played = played + 1, trueWins = trueWins + 1)
    case FalseWins        => copy(played = played + 1)
    case Draw | Undecided => copy(played = played + 1, trueWins = trueWins + 0.5f)
  }

  def unary_- = copy(trueWins = played - trueWins)

  def +(r: Results): Results = Results(played + r.played, trueWins + r.trueWins)

  def score: Float = trueWins / played
}