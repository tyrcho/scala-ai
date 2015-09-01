package games

import scala.util.Random
import scala.annotation.tailrec

object SimpleHSApp extends App {
  import Minions._
  val rush = Deck("rush", Map(minions(1) -> 10, minions(2) -> 10, minions(3) -> 10))
  val slow = Deck("slow", Map(minions(3) -> 10, minions(4) -> 10, minions(2) -> 10))

  println(List.fill(1000)(play(rush, rush, DefaultStrategy)).count(identity))

  def play(deck1: Deck, deck2: Deck, strat: Strategy): Boolean = { //true if d1 wins (d1 starts)
    val init1 = deck1.makeInitial(3)
    val init2 = deck2.makeInitial(4)
    val state = GameState(init1, init2)

    @tailrec
    def playStep(state: GameState, p1Turn: Boolean = true): Boolean = {
      val next = strat.playTurn(state)
      println(next)
      next.winner1 match {
        case Some(res) =>
          res != p1Turn
        case None => playStep(next, !p1Turn)
      }
    }

    playStep(state)
  }

}

trait Strategy {
  def playTurn(state: GameState): GameState
}

object DefaultStrategy extends Strategy {
  def playTurn(state: GameState): GameState = {
    val mine = state.state1.startTurn
    val opp = state.state2
    val played = // we only play 1 a turn
      mine.hand.filter(_.cost <= mine.mana).sortBy(-_.cost).headOption match {
        case Some(minion) => mine.play(minion)
        case None         => mine
      }
    val dmg = mine.inPlay.map(_.attack).sum
    val damaged = opp.damage(dmg)
    GameState(damaged, played)
  }
}

case class Minion(attack: Int, health: Int, cost: Int)

case class Deck(name: String, cards: Map[Minion, Int]) {
  def makeInitial(drawn: Int): PlayerBoardState = {
    val cardsList = for ((card, count) <- cards) yield List.fill(count)(card)
    PlayerBoardState(name, Random.shuffle(cardsList.flatten.toList)).draw(drawn)
  }
}

case class GameState(state1: PlayerBoardState, state2: PlayerBoardState) {
  def reverse = GameState(state2, state1)

  def winner1: Option[Boolean] =
    if (state1.health <= 0) Some(false)
    else if (state2.health <= 0) Some(true)
    else None

}

case class PlayerBoardState(
  name: String,
  deck: List[Minion],
  hand: List[Minion] = Nil,
  inPlay: List[Minion] = Nil,
  health: Int = 30,
  maxMana: Int = 0,
  mana: Int = 0) {

  def startTurn = {
    val max = math.min(10, maxMana + 1)
    val current = max
    val drawn :: rest = deck
    copy(maxMana = max,
      mana = current,
      deck = rest,
      hand = drawn :: hand)
  }

  def draw(count: Int = 1): PlayerBoardState =
    if (count >= 1) draw(count - 1).draw
    else this

  def draw: PlayerBoardState = {
    val drawn :: rest = deck
    copy(deck = rest,
      hand = drawn :: hand)
  }

  def damage(amount: Int) =
    copy(health = health - amount)

  def play(m: Minion) =
    copy(
      hand = hand.diff(List(m)),
      mana = mana - m.cost,
      inPlay = m :: inPlay)

  override def toString = s"$name : $health HP with ${hand.size} in hand and ${inPlay.size} in play.\n"
}

object Minions {
  val minions = (for {
    i <- 1 to 6
    att = i
    health = i + 1
    cost = i
  } yield i -> Minion(att, health, cost)).toMap
}