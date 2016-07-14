package geo

import geo.Geometry.Pos

case class FogGrid(
    width: Int = 1000,
    height: Int = 1000,
    scale: Int = 100,
    initialAge: Int = Int.MaxValue / 2,
    ages: Map[(Int, Int), Int] = Map.empty.withDefaultValue(Int.MaxValue / 2)) {

  def age(p: Pos) = ages(posToSpot(p))

  def updated(seen: Iterable[Pos], range: Double) = {
    def isSeen(p: Pos) = seen.exists(_.dist2(p) <= range * range)
    val newAges = for {
      (x, y) <- spots
      p = spotToPos(x, y)
      na = if (isSeen(p)) 0 else age(p) + 1
    } yield (x, y) -> na
    copy(ages = newAges.toMap)
  }

  private def posToSpot(p: Pos) = {
    val x = (p * scale / width).x.toInt
    val y = (p * scale / height).y.toInt
    (x, y)
  }

  private def spotToPos(x: Int, y: Int) =
    Pos(x * width / scale, y * height / scale)

  lazy val spots = for {
    i <- 0 until scale
    j <- 0 until scale
  } yield (i, j)

  def olderThan(a: Int): Seq[Pos] = for {
    (x, y) <- spots
    age = ages(x, y)
    if age > a
  } yield spotToPos(x, y)
}