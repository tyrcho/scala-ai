package geo

object Geometry {
  case class Pos(x: Double, y: Double) {
    def +(p: Pos) = Pos(x + p.x, y + p.y)
    def -(p: Pos) = Pos(x - p.x, y - p.y)
    def *(r: Double) = Pos(x * r, y * r)
    def /(r: Double) = Pos(x / r, y / r)
  }

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