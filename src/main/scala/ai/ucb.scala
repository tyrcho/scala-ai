package ai

import Ordering.by
import math.{ log, min, pow, sqrt }
import grizzled.slf4j.Logging

object ucb {

  type Arm[+R] = Function0[R]

  case class Stats[R, A <: Arm[R]](
    arm: A,
    plays: Int = 0,
    rewardSum: Double = 0,
    rewardSquaredSum: Double = 0,
    rewardAverage: Double = 0,
    rewardVariance: Double = 0) {

    def play(bandit: Bandit[R, A]): (R, Stats[R, A]) = {
      val plays = this.plays + 1
      val reward = arm()
      val normalizedReward = bandit.normalize(reward)
      val rewardSum = this.rewardSum + normalizedReward
      val rewardSquaredSum = this.rewardSquaredSum + pow(normalizedReward, 2)
      val rewardAverage = rewardSum / plays
      val rewardVariance = rewardSquaredSum / plays - pow(rewardAverage, 2)
      (reward, Stats(arm, plays, rewardSum, rewardSquaredSum, rewardAverage, rewardVariance))
    }

    def ucb(banditPlays: Int): Double =
      if (plays != 0) {
        val bias = log(banditPlays) / plays
        rewardAverage + sqrt(bias * min(0.25, rewardVariance + sqrt(bias * 2)))
      } else Double.PositiveInfinity

  }

  val byRewardAverage: Ordering[Stats[_, _]] = by(_.rewardAverage)

  case class Bandit[R, A <: Arm[R]](
    stats: List[Stats[R, A]],
    plays: Int,
    normalize: R => Double) extends Logging {

    debug(this)

    def this(arms: Traversable[A], normalize: R => Double) =
      this(arms.map(new Stats[R, A](_)).toList, 0, normalize)

    /**
     * Plays current best arm.
     * Returns the reward for this arm, and the updated Bandit.
     */
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

    def bestArm: A = stats.max(byRewardAverage).arm

  }
}