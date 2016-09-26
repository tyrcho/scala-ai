package geo

import org.scalatest.mock.MockitoSugar
import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import math._

@RunWith(classOf[JUnitRunner])
class PathsSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {

  "a Path" should "be short" in {
    val g = Grid(4, 4)
    import g.Point
    def validMoves(p: Point) = Seq(p.N, p.S, p.E, p.W)
    def targetCondition(p: Point) = p == Point(3, 0)
    val path = g.pathImpl(List((Point(0, 0), Nil)), Set.empty, validMoves, targetCondition)
    path shouldBe Some(List(Point(1, 0), Point(2, 0), Point(3, 0)))
  }

  "a Path" should "avoid obstacles" in {
    val g = Grid(4, 4)
    // ....
    // XXX.
    // XXX.
    // ....
    import g.Point
    def validMoves(p: Point) = Seq(p.N, p.S, p.E, p.W).filterNot(p => (p.y == 1 || p.y == 2) && p.x < 3)
    def targetCondition(p: Point) = p == Point(0, 3)
    val path = g.pathImpl(List((Point(0, 0), Nil)), Set.empty, validMoves, targetCondition)
    path shouldBe Some(List(
      Point(1, 0), Point(2, 0), Point(3, 0),
      Point(3, 1), Point(3, 2), Point(3, 3),
      Point(2, 3), Point(1, 3), Point(0, 3)))
  }

}