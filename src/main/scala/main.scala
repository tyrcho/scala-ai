import ai._
import ai.ucb._
import java.io.File

import Ordering.by
import util.Random.{nextDouble, shuffle}

import org.jfree.chart.ChartFactory.createXYLineChart
import org.jfree.chart.ChartUtilities.saveChartAsPNG
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.LogarithmicAxis
import org.jfree.chart.plot.PlotOrientation.VERTICAL
import org.jfree.data.xy.DefaultXYDataset

object Main {
  private val plays: Int = 10000
  private val trials: Int = 100

  private case class BernoulliArm(distribution: Double) extends Arm[Boolean] {
    override def play: Boolean = nextDouble <= distribution
  }

  def normalize(reward: Boolean): Double = if (reward) 1 else 0

  private def play(distributions: Double*): Array[Double] = {
    val arms: Traversable[BernoulliArm] = distributions.map(new BernoulliArm(_))
    val bestArm: BernoulliArm = arms.max(by(_.distribution): Ordering[BernoulliArm])
    val statistics: Array[Double] = new Array(plays)

    for (trial: Int <- 0 until trials) {
      var bandit = new Bandit[Boolean, BernoulliArm](shuffle(arms), normalize)
      for (play: Int <- 0 until plays) {
        if (bandit.bestArm == bestArm)
          statistics(play) += 1
        bandit = bandit.play._2
      }
    }
    statistics
  }

  private def plot(name: String, statistics: Array[Double]) {
    val series = Array.ofDim[Double](2, plays)
    for (play: Int <- 0 until plays) {
      series(0).update(play, play + 1)
      series(1).update(play, statistics(play) / trials)
    }
    val dataset = new DefaultXYDataset
    dataset.addSeries("UCB", series)
    val chart = createXYLineChart("UCB " + name, null, "probability best arm known", dataset, VERTICAL, false, false, false)
    chart.getXYPlot.setDomainAxis(new LogarithmicAxis("number of bandit plays"))
    saveChartAsPNG(new File(name + ".png"), chart, 600, 400)
  }

  def main(args: Array[String]) {
    plot("bandit1", play(0.9, 0.6))
    plot("bandit2", play(0.9, 0.8))
    plot("bandit3", play(0.55, 0.45))
    plot("bandit11", play(0.9, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6))
    plot("bandit12", play(0.9, 0.8, 0.8, 0.8, 0.7, 0.7, 0.7, 0.6, 0.6, 0.6))
    plot("bandit13", play(0.9, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8))
    plot("bandit14", play(0.55, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45))
  }
}
