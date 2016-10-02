package geo

object Geometry {

  import math._

  case class Pos(x: Double, y: Double) {
    def +(p: Pos) = Pos(x + p.x, y + p.y)
    def -(p: Pos) = Pos(x - p.x, y - p.y)
    def *(r: Double) = Pos(x * r, y * r)
    def /(r: Double) = Pos(x / r, y / r)
    def scalar(p: Pos) = x * p.x + y * p.y

    def norm: Double = dist(Pos(0, 0))
    def dist(other: Pos): Double = sqrt(dist2(other))
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

    def moveTo(tgt: Pos, distance: Double) =
      if (dist(tgt) <= distance) tgt
      else {
        val direction = tgt - this
        val distToTgt = sqrt(direction scalar direction)
        val delta = direction * (distance / distToTgt)
        this + delta
      }.round

    def symmetric(center: Pos) = center + center - this

    def round = Pos(x.toInt, y.toInt)

  }

  val MAX_ROTATION = 18
  val MAX_THRUST = 200
  val FRICTION = 0.85
  // angles
  val EAST = 0
  val SOUTH = 90
  val WEST = 180
  val NORTH = 270

  case class Entity(
      pos: Pos = Pos(0, 0),
      speed: Pos = Pos(0, 0),
      angle: Double = 0) {

    def move(target: Pos, thrust: Int): Entity = {
      rotateTo(target).
        accel(thrust).
        move(1).
        endTurn
    }

    def endTurn = copy(
      pos = pos.round,
      speed = (speed * FRICTION).round)

    def move(t: Double) =
      copy(pos = pos + speed * t)

    def accel(thrust: Int): Entity = {
      val t = thrust min MAX_THRUST
      val ra = angle.toRadians
      copy(speed = speed + Pos(
        x = cos(ra) * t,
        y = sin(ra) * t))
    }

    /*
     * Cette fonction renvoie l'angle que devrait avoir le pod pour faire face au point donné.
		 * Donc si le point se trouve par exemple exactement en haut à droite du pod, cette fonction donnera 315.
		 * 0 veut dire que le pod regarde plein est. 90 c'est plein sud. 180 plein ouest. Et enfin 270 plein nord. 
     */
    def deltaAngleToFace(p: Pos) = {
      val a = angleTo(p)

      // Pour connaitre le sens le plus proche, il suffit de regarder dans les 2 sens et on garde le plus petit
      // Les opérateurs ternaires sont la uniquement pour éviter l'utilisation d'un operateur % qui serait plus lent
      val right = if (angle <= a) a - angle else 360 - angle + a
      val left = if (angle >= a) angle - a else angle + 360 - a

      if (right <= left) right
      else -left // On donne un angle négatif s'il faut tourner à gauche
    }

    def angleTo(p: Pos) = {
      import pos._
      val d = dist2(p)
      val dx = (p.x - x) / sqrt(d)
      val dy = (p.y - y) / d

      val a = acos(dx).toDegrees

      // Si le point qu'on veut est en dessus de nous, il faut décaler l'angle pour qu'il soit correct.
      if (dy < 0) 360.0 - a
      else a
    }

    // L'opérateur % est lent. Si on peut l'éviter, c'est mieux.
    @inline private def fastMod360(a: Double) =
      if (a >= 360.0) a - 360.0
      else if (a < 0.0) a + 360.0
      else a

    // nouvel angle quand on essaie de viser p en partant de angle
    def rotateTo(p: Pos) = {
      val a = deltaAngleToFace(p)

      // On ne peut pas tourner de plus de 18° en un seul tour
      val b = angle + (if (a > MAX_ROTATION) MAX_ROTATION
      else if (a < -MAX_ROTATION) -MAX_ROTATION
      else a)
      copy(angle = fastMod360(b))
    }
  }

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