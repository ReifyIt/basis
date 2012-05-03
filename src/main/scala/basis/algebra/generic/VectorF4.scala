/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class VectorF4[F <: Ring { type Vector = F }] private
    (val Vector: VectorF4.Space[F], val x: F, val y: F, val z: F, val w: F)
  extends Vector4 {
  
  override type Vector = VectorF4[F]
  override type Scalar = F
}

object VectorF4 {
  def apply[F <: Ring { type Vector = F }]
      (Scalar: Ring.Space { type Vector = F }): Space[F] =
    new Space[F](Scalar)
  
  class Space[F <: Ring { type Vector = F }]
      (val Scalar: Ring.Space { type Vector = F })
    extends Ring.Scalar with Affine.Space with Vector4.Space {
    
    override type Point  = Vector
    override type Vector = VectorF4[F]
    override type Scalar = F
    
    override lazy val zero: Vector = super.zero
    
    override def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector =
      new Vector(this, x, y, z, w)
    
    override def toString: String = "F4"+"("+ Scalar +")"
  }
}
