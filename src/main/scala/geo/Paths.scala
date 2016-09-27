package geo

import scala.annotation.tailrec

case class Point(x: Int, y: Int) {
  def N = Point(x, y - 1)
  def S = Point(x, y + 1)
  def E = Point(x + 1, y)
  def W = Point(x - 1, y)

  def dist(o: Point) =
    (o.x - x) * (o.x - x) + (o.y - y) * (o.y - y)

  def closer(ref: Point)(o: Point) =
    dist(ref) < o.dist(ref)
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

  def path(from: Point, to: Point, validMoves: Point => Iterable[Point]) = {
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

    pathImpl(List((from, Nil)), Set.empty, validMoves, _ == to)
  }

  def destroyNextTurn(bombs: Set[Bomb], obstacles: Set[Point]): Set[Point] = {

    def destroyPointsOnDirection(bomb: Bomb, direction: Direction): Set[Point] = {

      @tailrec
      def destroyPointsOnDirectionRec(range: Int, destroyPoints: List[Point]): List[Point] = {
        if (range == 0) destroyPoints.filter(contains)
        else {
          val nextPoint = direction(destroyPoints.last)
          if (nextPoint.equals(destroyPoints.last) || obstacles.contains(nextPoint))
            destroyPoints
          else if (bombs.exists(b => b.position.equals(nextPoint)))
            destroyPoints :+ nextPoint
          else
            destroyPointsOnDirectionRec(range - 1, destroyPoints :+ nextPoint)
        }
      }

      destroyPointsOnDirectionRec(bomb.range, List(bomb.position)).toSet

    }

    @tailrec
    def destroyNextTurnRec(bombsLeft: Set[Bomb], bombsTreated: Set[Bomb], destroyPoints: Set[Point]): Set[Point] = {
      if (bombsLeft.isEmpty) destroyPoints
      else {
        val bomb = bombsLeft.head
        val newDestroyPoints: Set[Point] = List(N, S, W, E).foldLeft(Set.empty[Point]) {
          case (points: Set[Point], direction: Direction) => points ++ destroyPointsOnDirection(bomb, direction)
        }

        val newDestroyBombs: Set[Bomb] = bombs.filter(bomb => newDestroyPoints.contains(bomb.position) && !bombsLeft.contains(bomb) && !bombsTreated.contains(bomb))

        destroyNextTurnRec(bombsLeft - bomb ++ newDestroyBombs, bombsTreated + bomb, destroyPoints ++ newDestroyPoints)
      }
    }

    destroyNextTurnRec(bombs.filter(bomb => bomb.aboutToExplode), Set.empty[Bomb], Set.empty[Point])

  }
}

case class Bomb(position: Point, range: Int, nbTurns: Int) {

  def aboutToExplode: Boolean = nbTurns == 1

}

