/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A typeclass for a metric, or distance function, on a space. Use the
  * `distance` function in the package object to measure distances.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * scala> distance(PointR2(-1.0, 3.0), PointR2(2.0, 7.0))
  * res0: basis.algebra.Real = 5.0
  * }}}
  * 
  * @tparam Point   the point type of the metric space.
  * @tparam Scalar  the scalar type of the metric space.
  */
trait Metric[-Point, +Scalar] extends ((Point, Point) => Scalar) {
  /** Returns the distance between two points. */
  def apply(u: Point, v: Point): Scalar
}

/** A factory for creating `Metric` typeclasses. */
object Metric {
  /** Returns a new `Metric` typeclass wrapping the supplied function. */
  def apply[Point, Scalar](metric: (Point, Point) => Scalar) = new Metric[Point, Scalar] {
    def apply(p: Point, q: Point): Scalar = metric(p, q)
  }
}
