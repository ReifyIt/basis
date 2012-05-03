/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class VectorF2[F <: Ring { type Vector = F }] private
    (val Vector: VectorF2.Space[F], val x: F, val y: F)
  extends Vector2 {
  
  override type Vector = VectorF2[F]
  override type Scalar = F
}

object VectorF2 {
  def apply[F <: Ring { type Vector = F }]
      (Scalar: Ring.Space { type Vector = F }): Space[F] =
    new Space[F](Scalar)
  
  class Space[F <: Ring { type Vector = F }]
      (val Scalar: Ring.Space { type Vector = F})
    extends Ring.Scalar with Affine.Space with Vector2.Space {
    
    override type Point  = Vector
    override type Vector = VectorF2[F]
    override type Scalar = F
    
    override lazy val zero: Vector = super.zero
    
    override def apply(x: Scalar, y: Scalar): Vector =
      new Vector(this, x, y)
    
    override def toString: String = "F2"+"("+ Scalar +")"
  }
}
