/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

import language.existentials

class F4[S <: Ring with Singleton](val Scalar: S) extends Vector4Space[S] {
  final class Element(val x: Scalar, val y: Scalar, val z: Scalar, val w: Scalar) extends super.Element {
    def / [E <: F#Element forSome { type F <: Field { type Vector = Scalar } }]
        (scalar: Scalar)(implicit isField: Scalar <:< E): Vector =
      Vector(x / scalar, y / scalar, z / scalar, w / scalar)
    
    def norm[E <: F#Element forSome { type F <: CompleteField { type Vector = Scalar } }]
        (implicit isCompleteField: Scalar <:< E): Scalar =
      (this ⋅ this).sqrt
    
    def normalized[E <: F#Element forSome { type F <: CompleteField { type Vector = Scalar } }]
        (implicit isCompleteField: Scalar <:< E): Vector =
      this / norm
  }
  
  override type Vector = Element
  
  override lazy val zero: Vector = super.zero
  
  override def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector =
    new Vector(x, y, z, w)
  
  override def toString: String = "F4"+"("+ Scalar +")"
}
