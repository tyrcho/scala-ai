import math._
import scala.util._
import scala.io.StdIn._
import scala.annotation.tailrec

object CodingGameTemplate {
  implicit class MinMaxOptionIterable[T](it: Iterable[T]) {
    def minOption[O: Ordering](by: T => O): Option[T] =
      if (it.isEmpty) None
      else Some(it.minBy(by))

    def maxOption[O: Ordering](by: T => O): Option[T] =
      if (it.isEmpty) None
      else Some(it.maxBy(by))
  }
  def sqr(x: Double) = x * x

  def debug(m: => Any) = Console.err.println(m)
}