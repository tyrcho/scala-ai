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
    def validMoves(p: Point) = g.neighbours(p)
    val path = g.path(Point(0, 0), Point(3, 0), validMoves)
    path shouldBe Some(List(Point(1, 0), Point(2, 0), Point(3, 0)))
  }

  "a Path" should "avoid obstacles" in {
    val g = Grid(4, 4)
    // ....
    // XXX.
    // XXX.
    // ....
    def validMoves(p: Point) = g.neighbours(p).filterNot(p => (p.y == 1 || p.y == 2) && p.x < 3)
    val path = g.path(Point(0, 0), Point(0, 3), validMoves)
    path shouldBe Some(List(
      Point(1, 0), Point(2, 0), Point(3, 0),
      Point(3, 1), Point(3, 2), Point(3, 3),
      Point(2, 3), Point(1, 3), Point(0, 3)))
  }

  "all paths" should "find some paths" in {
    val g = Grid(3, 3)
    def validMoves(p: Point, t: Int) = g.neighbours(p).toSet 
    val path = g.allPaths(Point(0, 0), validMoves, 1)
    path shouldBe List(List(Point(1, 0)), List(Point(0, 1)))
  }

  "all paths" should "find some paths with more depth" in {
    val g = Grid(3, 3)
    def validMoves(p: Point, t: Int) = g.neighbours(p)
    val path = g.allPaths(Point(0, 0), validMoves, 2)
    path shouldBe List(
      List(Point(2, 0), Point(1, 0)),
      List(Point(1, 1), Point(0, 1)),
      List(Point(0, 2), Point(0, 1)),
      List(Point(1, 0)),
      List(Point(0, 1)))
  }

  "all paths" should "find avoid obstacles" in {
    // .X.
    // ...
    // ...
    val g = Grid(3, 3)
    def validMoves(p: Point, t: Int) = g.neighbours(p).toSet - Point(1,0)
    val path = g.allPaths(Point(0, 0), validMoves, 2)
    path shouldBe List(
      List(Point(1, 1), Point(0, 1)),
      List(Point(0, 2), Point(0, 1)),
      List(Point(0, 1)))
  }

}