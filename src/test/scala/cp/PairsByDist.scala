package cp

import scala.util.Random

object Demo extends App {
  val students = Vector.fill(16)(Pos(Random.nextInt(1000), Random.nextInt(1000)))
  val solver = PairsByDist(students)
  val e = solver.solution
  println(solver.badSolution.score + " " + e.score + " " + e)

  implicit class SolutionOps(s: Vector[(Pos, Pos)]) {
    def score = s.foldLeft(0.0) {
      case (total, (p1, p2)) => total + p1.dist(p2)
    }
  }

  case class PairsByDist(students: Vector[Pos]) {
    def badSolution = students.sliding(2, 2).map { case Vector(a, b) => (a, b) }.toVector
    def solution: Vector[(Pos, Pos)] =
      if (students.size == 2) Vector((students(0), students(1)))
      else {
        val others = students.drop(2)
        val previousSol = PairsByDist(others).solution
        val (s1, s2) = (students(0), students(1))
        val closerToS1 = others.minBy(_.dist2(s1))
        val s1Steals: PartialFunction[(Pos, Pos), Pos] = {
          case (`closerToS1`, a) => a
          case (a, `closerToS1`) => a
        }
        val stolenByS1 = previousSol.collect(s1Steals).head
        previousSol.filterNot(s1Steals.isDefinedAt) :+ (closerToS1, s1) :+ (stolenByS1, s2)
      }
  }

  case class Pos(x: Int, y: Int) {
    def dist2(p: Pos) = p.x * x + p.y * y
    def dist(p: Pos) = math.sqrt(dist2(p))
  }
}

