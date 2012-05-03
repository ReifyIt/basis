/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class VectorF3[F <: Ring { type Vector = F }] private
    (val Vector: VectorF3.Space[F], val x: F, val y: F, val z: F)
  extends Vector3 {
  
  override type Vector = VectorF3[F]
  override type Scalar = F
}

object VectorF3 {
  def apply[F <: Ring { type Vector = F }]
      (Scalar: Ring.Space { type Vector = F }): Space[F] =
    new Space[F](Scalar)
  
  class Space[F <: Ring { type Vector = F }]
      (val Scalar: Ring.Space { type Vector = F })
    extends Ring.Scalar with Affine.Space with Vector3.Space {
    
    override type Point  = Vector
    override type Vector = VectorF3[F]
    override type Scalar = F
    
    override lazy val zero: Vector = super.zero
    
    override def apply(x: Scalar, y: Scalar, z: Scalar): Vector =
      new Vector(this, x, y, z)
    
    override def toString: String = "F3"+"("+ Scalar +")"
  }
}
