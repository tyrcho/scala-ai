package games

import grizzled.slf4j.Logging
import scala.util.Random
import scala.collection.parallel.ParSeq

object HsCurveAnalysis extends App with Logging {
  type Curve = Map[Int, Int]
  type Result = Int
  type Card = Int
  case class Simulation(curve: Curve, turn: Int)

  implicit class CurveOps(curve: Curve) {
    def +(other: Curve): Curve =
      (for {
        i <- 0 to 10
        c = curve(i)
        o = other(i)
        newCount = (c + o + 1) / 2
      } yield i -> newCount).toMap

    def breed(other: Curve) =
      (curve + other).mutate.norm

    def mutate: Curve = {
      val temp = curve.mapValues(_ + 2)
      val cards = temp.deck
      val more = cards.head
      val less = cards.find(_ != more).get
      curve.updated(more, curve(more) + 1).updated(less, curve(less) - 1)
    }

    def norm: Curve =
      if (curve.values.sum > 30)
        (curve - deck.head).norm.withDefaultValue(0)
      else curve

    def deck: List[Card] = util.Random.shuffle((for {
      (cost, count) <- curve
    } yield List.fill(count.toInt)(cost)).flatten.toList)

    def avgMiss(turn: Int, repeats: Int = 1000): Double =
      averageMiss(Simulation(curve, turn), repeats)

    val total = curve.values.sum

    def print: String = (for (i <- curve.keys.min to curve.keys.max) yield s"$i : " + "*" * curve(i).toInt).mkString("\n")
  }

  def averageMiss(sim: Simulation, repeats: Int = 1000): Double = {
    List.fill(repeats)(play(sim).toLong).sum / repeats.toDouble
  }

  def play(sim: Simulation): Result = {
    val initial = sim.curve.deck
    debug(initial.toString)

    val coin = util.Random.nextBoolean
    val initialSize = 3 + (if (coin) 1 else 0)
    val hand = initial.take(initialSize)
    val delta = playTurn(hand, initial.drop(initialSize), 1, sim.turn, 0, coin)
    debug(s"$delta for $sim")
    delta
  }

  def playTurn(hand: List[Card], deck: List[Card], turn: Int, max: Int, res: Result, hasCoin: Boolean): Result =
    if (turn > max) res else {
      val newHand = deck.head :: hand
      val played = bestCards(newHand, turn, Nil, hasCoin)
      val usedMana = played.sum
      val usedCoin = usedMana > turn
      val stillCoin = hasCoin && !usedCoin
      debug(s"turn $turn : hand $newHand | played $played")
      val cost = turn - usedMana + (if (usedCoin) 1 else 0)
      playTurn(newHand diff played, deck.tail, turn + 1, max, res + cost, stillCoin)
    }

  def bestCards(hand: List[Card], available: Int, chosen: List[Card] = Nil, hasCoin: Boolean): List[Card] = {
    val availWithCoin = available + (if (hasCoin) 1 else 0)
    val candidates = hand.filter(_ <= availWithCoin)
    if (candidates.isEmpty) chosen
    else {
      candidates.filter(_ == available).headOption match {
        case Some(best) => best :: chosen //exactly the cost
        case None =>
          candidates.filter(_ == availWithCoin).headOption match {
            case Some(bestWithCoin) => bestWithCoin :: chosen
            case None =>
              val m = candidates.filter(_ <= available).max
              bestCards(hand diff List(m), available - m, m :: chosen, false) // only use coin for max card
          }
      }

    }
  }

  def nextGen1(selected: List[Curve]): Curve = {
    val f = selected(Random.nextInt(selected.size))
    val m = selected(Random.nextInt(selected.size))
    f.breed(m)
  }

  def nextGen(selected: List[Curve]): List[Curve] = {
    selected ::: List.fill(poolSize - selected.size)(nextGen1(selected))
  }

  def select(curves: List[Curve]) =
    curves.sortBy(_.avgMiss(keyTurn)).dropRight(6)

  val keyTurn = 7
  val sampleCurve = Map[Int, Int](1 -> 4, 2 -> 6, 3 -> 6, 4 -> 6, 5 -> 4, 6 -> 2, 7 -> 2).withDefaultValue(0)
  val sampleSim = Simulation(sampleCurve, keyTurn)
  val cost = averageMiss(sampleSim)
  println(sampleCurve.print)
  println(f"avg miss $cost%.1f")

  val poolSize = 20
  println(sampleCurve.avgMiss(keyTurn))
  var generation = 1

  var curves = List.fill(poolSize)(sampleCurve.mutate)
  for (i <- 1 to 1000) {
    val selected = curves.sortBy(_.avgMiss(keyTurn, 30 * generation)).dropRight(poolSize * 3 / 4)
    curves = nextGen(selected)
    println(selected.head.print)
    println(generation + " => " + selected.head.avgMiss(keyTurn))
    generation += 1
  }
  val best = curves.head
  println(best.avgMiss(keyTurn))
  println(best.print)
}
