package games

import grizzled.slf4j.Logging

object HsCurveAnalysis extends App with Logging {
  type Curve = Map[Int, Int]
  type Result = Int
  type Card = Int
  val sampleCurve = Map(1 -> 6, 2 -> 6, 3 -> 6, 4 -> 6, 5 -> 4).withDefaultValue(0)
  case class Simulation(curve: Curve, turn: Int)

  val sampleSim = Simulation(sampleCurve, 7)

  def averageCost(sim: Simulation, repeats: Int = 10000): Double = {
    List.fill(repeats)(play(sim)).sum / repeats.toDouble
  }

  def play(sim: Simulation): Result = {
    val initial = util.Random.shuffle((for {
      (cost, count) <- sim.curve
    } yield List.fill(count)(cost)).flatten.toList)
    debug(initial.toString)

    val initialSize = 3
    val hand = initial.take(initialSize)
    playTurn(hand, initial.drop(initialSize), 1, sim.turn, 0)
  }

  def playTurn(hand: List[Card], deck: List[Card], turn: Int, max: Int, res: Result): Result =
    if (turn > max) res else {
      val newHand = deck.head :: hand
      val played = bestCards(newHand, turn)
      debug(s"turn $turn : hand $newHand | played $played")
      val cost = turn - played.sum
      playTurn(newHand diff played, deck.tail, turn + 1, max, res + cost)
    }

  def bestCards(hand: List[Card], available: Int, chosen: List[Card] = Nil): List[Card] = {
    val candidates = hand.filter(_ <= available)
    if (candidates.isEmpty) chosen
    else {
      val m = candidates.max
      bestCards(hand diff List(m), available - m, m :: chosen)
    }
  }

  val cost = averageCost(sampleSim)
  println(f"avg cost $cost%.1f")
}
