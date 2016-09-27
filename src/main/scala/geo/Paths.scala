package geo

import scala.annotation.tailrec

case class Grid(width: Int, height: Int) {

  case class Point(x: Int, y: Int) {
    def N = Point(x, y - 1).check
    def S = Point(x, y + 1).check
    def E = Point(x + 1, y).check
    def W = Point(x - 1, y).check

    def check: Point =
      if (x < 0) Point(0, y).check
      else if (x >= width) Point(width - 1, y).check
      else if (y < 0) Point(x, 0).check
      else if (y >= height) Point(x, height - 1).check
      else this

    def neighbours = List(N, S, E, W)

    def dist(o: Point) =
      (o.x - x) * (o.x - x) + (o.y - y) * (o.y - y)

    def closer(ref: Point)(o: Point) =
      dist(ref) < o.dist(ref)
  }

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
      case (path, le) :: tail if !explored(path.head) && le <= maxDepth  =>
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

  //  def path(from: Point, to: Cell) =
  //    pathImpl(List((from, Nil)), Set.empty, data.get(_) == Some(to))
  //
  //  def path(from: Point, test: Cell => Boolean) =
  //    pathImpl(List((from, Nil)), Set.empty, p => data.get(p).map(test).getOrElse(false))

}