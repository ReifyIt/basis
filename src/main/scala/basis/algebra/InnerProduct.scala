/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A typeclass for an inner product on a vector space.
  * 
  * @author Chris Sachs
  * 
  * @tparam Vector  the vector type of the inner product space.
  * @tparam Scalar  the scalar type of the inner product space.
  */
trait InnerProduct[-Vector, +Scalar] extends ((Vector, Vector) => Scalar) {
  /** Returns the inner product of two vectors. */
  def apply(u: Vector, v: Vector): Scalar
}

/** A factory for creating `InnerProduct` typeclasses. */
object InnerProduct {
  /** Returns a new `InnerProduct` typeclass wrapping the supplied function. */
  def apply[Vector, Scalar](innerProduct: (Vector, Vector) => Scalar) = new InnerProduct[Vector, Scalar] {
    def apply(u: Vector, v: Vector): Scalar = innerProduct(u, v)
  }
}
