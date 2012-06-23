/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

import language.existentials

/** A generic space of 4-dimensional vectors over a ring.
  * 
  * @author Chris Sachs
  * 
  * @tparam S    The scalar set of this $space.
  */
class F4[S <: Ring with Singleton](val Scalar: S) extends Vector4Space[S] {
  final class Element(val x: Scalar, val y: Scalar, val z: Scalar, val w: Scalar) extends super.Element {
    /** Returns the quotient of this $vector divided by a $scalar.
      * 
      * @usecase def / (scalar: Scalar): Vector
      *   @inheritdoc
      */
    def / [E <: F#Element forSome { type F <: Field { type Value = Scalar } }]
        (scalar: Scalar)(implicit isField: Scalar <:< E): Vector =
      Vector(x / scalar, y / scalar, z / scalar, w / scalar)
    
    /** Returns the Euclidean norm of this $vector.
      * 
      * @usecase def norm: Scalar
      *   @inheritdoc
      */
    def norm[E <: F#Element forSome { type F <: CompleteField { type Value = Scalar } }]
        (implicit isCompleteField: Scalar <:< E): Scalar =
      (this ⋅ this).sqrt
    
    /** Returns a $vector in the same direction as this $vector but scaled to unit length.
      * 
      * @usecase def normalized: Vector
      *   @inheritdoc
      */
    def normalized[E <: F#Element forSome { type F <: CompleteField { type Value = Scalar } }]
        (implicit isCompleteField: Scalar <:< E): Vector =
      this / norm
  }
  
  override type Vector = Element
  
  override lazy val zero: Vector = super.zero
  
  override def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector = new Vector(x, y, z, w)
  
  override def toString: String = "F4"+"("+ Scalar +")"
}
