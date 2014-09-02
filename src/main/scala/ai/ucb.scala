package ai

import Ordering.by
import math.{log, min, pow, sqrt}

trait Arm[R] {
  def play: R
}

final class Stats[R, A <: Arm[R]]
    (val arm: A,
     val plays: Int,
     val rewardSum: Double,
     val rewardSquaredSum: Double,
     val rewardAverage: Double,
     val rewardVariance: Double) {

  def this(arm: A) = this(arm, 0, 0, 0, 0, 0)

  def play(bandit: Bandit[R, A]): (R, Stats[R, A]) = {
    val plays = this.plays + 1
    val reward = arm.play
    val normalizedReward = bandit.normalize(reward)
    val rewardSum = this.rewardSum + normalizedReward
    val rewardSquaredSum = this.rewardSquaredSum + pow(normalizedReward, 2)
    val rewardAverage = rewardSum / plays
    val rewardVariance = rewardSquaredSum / plays - pow(rewardAverage, 2)
    (reward, new Stats[R, A](arm, plays + 1, rewardSum, rewardSquaredSum, rewardAverage, rewardVariance))
  }

  def ucb(banditPlays: Int): Double = {
    if (plays != 0) {
      val bias: Double = log(banditPlays) / plays
      rewardAverage + sqrt(bias * min(0.25, rewardVariance + sqrt(bias * 2)))
    }
    else Double.PositiveInfinity
  }

}

object Stats {
  val byRewardAverage: Ordering[Stats[_, _]] = by(_.rewardAverage)
}

final class Bandit[R, A <: Arm[R]]
    (val stats: List[Stats[R, A]],
     val plays: Int,
     val normalize: R => Double) {

  def this(arms: Traversable[A], normalize: R => Double) =
    this(arms.map(new Stats[R, A](_)).toList, 0, normalize)

  def play: (R, Bandit[R, A]) =
    stats.head.play(this) match {
      case (reward, updated) => {
        val plays = this.plays + 1
        val stats = this.stats.tail.span(updated.ucb(plays) < _.ucb(plays)) match {
          case (lhs, rhs) => lhs ::: updated :: rhs
        }
        (reward, new Bandit(stats, plays, normalize))
      }
    }

  def bestArm: A = stats.max(Stats.byRewardAverage).arm

}