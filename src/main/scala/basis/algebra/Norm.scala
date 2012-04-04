/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A typeclass for a norm on a vector space.
  * 
  * @author Chris Sachs
  * 
  * @tparam Vector  the vector type of the normed vector space.
  * @tparam Scalar  the scalar type of the normed vector space.
  */
trait Norm[-Vector, +Scalar] extends (Vector => Scalar) {
  /** Returns the norm of a vector. */
  def apply(u: Vector): Scalar
}

/** A factory for creating `Norm` typeclasses. */
object Norm {
  /** Returns a new `Norm` typeclass wrapping the supplied function. */
  def apply[Vector, Scalar](norm: Vector => Scalar) = new Norm[Vector, Scalar] {
    def apply(u: Vector): Scalar = norm(u)
  }
}
