package geo

object Geometry {
  case class Pos(x: Double, y: Double) {
    def +(p: Pos) = Pos(x + p.x, y + p.y)
    def -(p: Pos) = Pos(x - p.x, y - p.y)
    def *(r: Double) = Pos(x * r, y * r)
    def /(r: Double) = Pos(x / r, y / r)

    def dist(other: Pos): Double = math.sqrt(dist2(other))
    def dist2(other: Pos): Double = sqr(this.x - other.x) + sqr(this.y - other.y)

    // point of line a-b closest to this
    def closest(a: Pos, b: Pos) = {
      val da = b.y - a.y
      val db = a.x - b.x
      val c1 = db * a.x + db * a.y
      val c2 = -db * x + da * y
      val det = sqr(da) + sqr(db)
      if (det != 0)
        Pos((da * c1 - db * c2) / det, (da * c2 + db * c1) / det)
      else Pos(x, y)
    }
  }

  case class Entity(pos: Pos, speed: Pos, accel: Pos)

  def sqr(x: Double) = x * x

  /*
    case class GenPos[T: Numeric](x: T, y: T) {
    val num = implicitly[Numeric[T]]
    implicit class RichT(e: T) {
      @inline def +(f: T) = num.plus(e, f)
      @inline def -(f: T) = num.plus(e, f)
    }
    def +(p: GenPos[T]) = GenPos[T](x + p.x, y + p.y)
    def -(p: GenPos[T]) = GenPos[T](x - p.x, y - p.y)
  }
   */
}