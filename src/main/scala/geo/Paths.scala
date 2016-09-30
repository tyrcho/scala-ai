package geo

import scala.annotation.tailrec
import math._

case class Point(x: Int, y: Int) {
  def N = Point(x, y - 1)
  def S = Point(x, y + 1)
  def E = Point(x + 1, y)
  def W = Point(x - 1, y)

  def dist(other: Point): Double = sqrt(dist2(other))
  def dist2(other: Point): Double = sqr(this.x - other.x) + sqr(this.y - other.y)

  def closer(ref: Point)(o: Point) =
    dist(ref) < o.dist(ref)

  override def toString = s"$x $y"

  def sqr(x: Int) = x * x
}

case class Grid(width: Int, height: Int) {

  def contains(p: Point): Boolean =
    p.x >= 0 && p.y >= 0 && p.x < width && p.y < height

  def neighbours(p: Point) = List(p.N, p.S, p.E, p.W).filter(contains)

  type Direction = Point => Point

  val N: Direction = _.N
  val S: Direction = _.S
  val E: Direction = _.E
  val W: Direction = _.W

  type Path = List[Point]

  def reachableCross(p: Point, range: Int, stops: Point => Boolean): Set[Point] = {
    @tailrec
    def reachableLine(from: Point, dir: Point => Point, range: Int, acc: Set[Point] = Set.empty): Set[Point] =
      if (range < 0) acc
      else if ((stops(from) && from != p) || !contains(dir(from))) acc + from
      else reachableLine(dir(from), dir, range - 1, acc + from)

    reachableLine(p, _.N, range) ++
      reachableLine(p, _.S, range) ++
      reachableLine(p, _.E, range) ++
      reachableLine(p, _.W, range)
  }

  def reachableArea(from: Point, validMoves: (Point) => Iterable[Point]): Set[Point] = {

    @tailrec
    def reachRec(toExplore: Vector[Point], explored: Set[Point]): Set[Point] =
      if (toExplore.isEmpty) explored
      else {
        val next = validMoves(toExplore.head).toSet -- explored
        reachRec(toExplore.tail ++ next, explored ++ next)
      }
    reachRec(Vector(from), Set.empty)
  }

  def border(p: Point) = p.x == 0 || p.y == 0 || p.x == width - 1 || p.y == height - 1

  def allPaths(from: Point, validMoves: (Point, Int) => Iterable[Point], maxDepth: Int): Iterable[Path] = {
    @tailrec
    def allPathsImpl(
      toExplore: List[(Path, Int)],
      explored: Set[Point],
      validMoves: (Point, Int) => Iterable[Point],
      maxDepth: Int,
      acc: List[Path] = Nil): List[Path] = toExplore match {
      case Nil => acc
      case (path, le) :: tail if !explored(path.head) && le <= maxDepth =>
        val newAcc = if (le > 0) path.init :: acc else acc
        val newPoints = for {
          m <- validMoves(path.head, path.length)
          //        if !tail.flatten.toSet(m)
          if !explored(m)
        } yield m
        val newPaths = newPoints.map(po => (po :: path, le + 1)).toList
        allPathsImpl(tail ++ newPaths, explored + path.head, validMoves, maxDepth, newAcc)
      case p :: tail => allPathsImpl(tail, explored, validMoves, maxDepth, acc)
    }

    allPathsImpl(List((List(from), 0)), Set.empty, validMoves, maxDepth)
  }

  def path(from: Point, to: Point, validMoves: Point => Iterable[Point], maxDepth: Int = 10) = {
    @tailrec
    def pathImpl(
      toExplore: List[(Point, Path)],
      explored: Set[Point],
      validMoves: Point => Iterable[Point],
      targetCondition: Point => Boolean,
      maxDepth: Int = 10): Option[Path] =

      toExplore match {
        case Nil                                      => None
        case (to, path) :: _ if targetCondition(to)   => Some(path.reverse)
        case (_, path) :: _ if path.length > maxDepth => None
        case (p, path) :: t =>
          def alreadyQueued(n: Point) = toExplore.exists {
            case (`n`, _) => true
            case _        => false
          }

          val newPoints = for {
            m <- validMoves(p)
            if !alreadyQueued(m)
            if !explored.contains(m)
          } yield (m, m :: path)
          pathImpl(t ++ newPoints, explored + p, validMoves, targetCondition, maxDepth)
      }

    pathImpl(List((from, Nil)), Set.empty, validMoves, _ == to, maxDepth)
  }

  def destroyBeforeTurn(turn: Int, bombs: Set[Bomb], walls: Set[Point], boxes: Set[Point]): Set[Point] =
    if (turn == 0) Set.empty
    else {
      val previouslyDestroyed = destroyBeforeTurn(turn - 1, bombs, walls, boxes)
      val remainingBombs = bombs.collect {
        case b if !previouslyDestroyed(b.position) => b.copy(nbTurns = b.nbTurns + 1 - turn)
      }
      val remainingBoxes = boxes -- previouslyDestroyed
      previouslyDestroyed ++ destroyNextTurn(remainingBombs, walls ++ remainingBoxes) -- walls
    }

  def destroyNextTurn(bombs: Set[Bomb], obstacles: Set[Point]): Set[Point] = {

    def destroyPointsOnDirection(bomb: Bomb, direction: Direction): Set[Point] = {

      @tailrec
      def destroyPointsOnDirectionRec(range: Int, acc: List[Point]): List[Point] = {
        if (range == 0) acc.filter(contains)
        else {
          val nextPoint = direction(acc.last)
          if (nextPoint.equals(acc.last))
            acc
          else if (obstacles(nextPoint) || bombs.exists(_.position == nextPoint))
            acc :+ nextPoint
          else
            destroyPointsOnDirectionRec(range - 1, acc :+ nextPoint)
        }
      }

      destroyPointsOnDirectionRec(bomb.range, List(bomb.position)).toSet

    }

    @tailrec
    def destroyNextTurnRec(bombsLeft: Set[Bomb], bombsTreated: Set[Bomb], destroyPoints: Set[Point]): Set[Point] = {
      if (bombsLeft.isEmpty) destroyPoints
      else {
        val bomb = bombsLeft.head
        val newDestroyPoints = List(N, S, W, E).foldLeft(Set.empty[Point]) {
          case (points, direction) => points ++ destroyPointsOnDirection(bomb, direction)
        }

        val newDestroyBombs = bombs.filter(bomb => newDestroyPoints.contains(bomb.position) && !bombsLeft.contains(bomb) && !bombsTreated.contains(bomb))

        destroyNextTurnRec(bombsLeft - bomb ++ newDestroyBombs, bombsTreated + bomb, destroyPoints ++ newDestroyPoints)
      }
    }

    destroyNextTurnRec(bombs.filter(bomb => bomb.aboutToExplode), Set.empty[Bomb], Set.empty[Point])

  }
}

case class Bomb(position: Point, range: Int, nbTurns: Int) {

  def aboutToExplode: Boolean = nbTurns == 1

  def nextTurn = copy(nbTurns = nbTurns - 1)
}

