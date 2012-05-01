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
  extends Vector4.Template {
  
  override type Vector = VectorF4[F]
  override type Scalar = F
}

object VectorF4 {
  def apply[F <: Ring { type Vector = F }] = Space.asInstanceOf[Space[F]]
  
  private val Space = new Space[Nothing]
  
  class Space[F <: Ring { type Vector = F }] extends Vector4.Space {
    override type Vector = VectorF4[F]
    override type Scalar = F
    
    override def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector =
      new Vector(this, x, y, z, w)
    
    override def toString: String = "F4"
  }
}
