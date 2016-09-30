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
    def validMoves(p: Point, t: Int) = g.neighbours(p).toSet - Point(1, 0)
    val path = g.allPaths(Point(0, 0), validMoves, 2)
    path shouldBe List(
      List(Point(1, 1), Point(0, 1)),
      List(Point(0, 2), Point(0, 1)),
      List(Point(0, 1)))
  }

  "a bomb" should "blast points in its range including obstacles" in {
    val g = Grid(5, 5)
    val bombs = Set(Bomb(Point(1, 2), 2, 1))
    val obstacles = Set(Point(1, 0))
    // .X...
    // .....
    // .B...
    // .....
    // .....
    g.destroyNextTurn(bombs, obstacles) shouldBe Set(Point(1, 0), Point(1, 1), Point(1, 2), Point(1, 3), Point(1, 4), Point(0, 2), Point(2, 2), Point(3, 2))
  }

  "a bomb" should "not destroy behind  obstacles" in {
    val g = Grid(5, 5)
    import g._
    val bombs = Set(Bomb(Point(1, 2), 2, 1))
    val obstacles = Set(Point(1, 0), Point(2, 2))
    // .X...
    // .....
    // .BX..
    // .....
    // .....
    g.destroyNextTurn(bombs, obstacles) shouldBe Set(Point(1, 1), Point(1, 2), Point(1, 3), Point(1, 4), Point(0, 2)) ++ obstacles
  }

  "a bomb" should "not explode before its turn" in {
    val g = Grid(5, 5)
    import g._
    val bombs = Set(Bomb(Point(1, 2), 2, 1), Bomb(Point(2, 3), 2, 5))
    val obstacles = Set(Point(1, 0), Point(2, 2))
    // .X...
    // .....
    // .BX..
    // ..B..
    // .....
    g.destroyNextTurn(bombs, obstacles) shouldBe Set(Point(1, 1), Point(1, 2), Point(1, 3), Point(1, 4), Point(0, 2)) ++ obstacles
  }

  "a bomb explosion" should "trigger other bomb" in {
    val g = Grid(5, 5)
    val bombs = Set(Bomb(Point(1, 2), 2, 1), Bomb(Point(1, 3), 2, 8))
    val obstacles = Set(Point(1, 0), Point(2, 2))
    // .X...
    // .....
    // .BX..
    // .B...
    // .....
    g.destroyNextTurn(bombs, obstacles) shouldBe Set(Point(1, 1), Point(1, 2), Point(1, 3), Point(1, 4), Point(0, 2), Point(0, 3), Point(2, 3), Point(3, 3)) ++ obstacles
  }

  "trigger an other bomb" should "reduce the range" in {
    val g = Grid(5, 6)
    val bombs = Set(Bomb(Point(1, 2), 3, 1), Bomb(Point(1, 3), 1, 8))
    val obstacles = Set(Point(1, 0), Point(2, 2))
    // .X...
    // .....
    // .BX..
    // .B...
    // .....
    // .....
    g.destroyNextTurn(bombs, obstacles) shouldBe Set(Point(1, 1), Point(1, 2), Point(1, 3), Point(1, 4), Point(0, 2), Point(0, 3), Point(2, 3)) ++ obstacles
  }

  "2 bombs" should "detonate one after the other" in {
    val g = Grid(5, 6)
    val bombs = Set(Bomb(Point(1, 2), 3, 1), Bomb(Point(2, 3), 3, 2))
    val walls = Set(Point(1, 0), Point(2, 2))
    val boxes = Set(Point(1, 3))
    // .X...
    // .....
    // .BX..
    // .oB..
    // .....
    // .....
    g.destroyBeforeTurn(1, bombs, walls, boxes) shouldBe Set(Point(1, 1), Point(1, 2), Point(1, 3), Point(0, 2))
    g.destroyBeforeTurn(2, bombs, walls, boxes) shouldBe
      Set(Point(1, 1), Point(1, 2), Point(1, 3), Point(0, 2)) ++
      Set(Point(0, 3), Point(2, 3), Point(3, 3), Point(4, 3), Point(2, 4), Point(2, 5))
  }

  "reachable area" should "find relevant points" in {
    // S..
    // .XX
    // .X.
    val g = Grid(3, 3)
    val obstacles = Set(Point(1, 1), Point(1, 2), Point(2, 1))
    def validMoves(p: Point) = g.neighbours(p).toSet -- obstacles
    g.reachableArea(Point(0, 0), validMoves) shouldBe Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(1, 0), Point(2, 0))
  }

  "reachable cross" should "work in the corner" in {
    val g = Grid(3, 3)
    // SX.
    // ...
    // ...
    def stops(p: Point) = p == Point(1, 0)
    g.reachableCross(Point(0, 0), 2, stops) shouldBe Set(Point(0, 0), Point(0, 1), Point(1, 0), Point(0, 2))
  }

  "reachable cross" should "work in the center" in {
    val g = Grid(5, 5)
    def stops(p: Point) = p == Point(3, 3)
    g.reachableCross(Point(3, 3), 1, stops) shouldBe Set(Point(3, 3), Point(3, 4), Point(3, 2), Point(2, 3), Point(4, 3))
  }

}