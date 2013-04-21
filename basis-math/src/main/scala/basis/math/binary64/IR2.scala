/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A 2-dimensional double-precision floating-point interval vector space.
  * 
  * @author Chris Sachs
  * @since  0.1
  * @group  Real
  */
object IR2 extends AffineSpace with IRN with F2 {
  final class Value(
      override val x: Scalar,
      override val y: Scalar)
    extends super[AffineSpace].Value
       with super[IRN].Value
       with super[F2].Value {
    
    def contains(vector: R2.Vector): Boolean =
      x.contains(vector.x) && y.contains(vector.y)
  }
  
  override type Point = Value
  
  override type Vector = Value
  
  override val Vector: IR2.type = IR2
  
  override type Scalar = RealInterval
  
  override val Scalar: RealInterval.type = RealInterval
  
  override def origin: Vector = zero
  
  override val zero: Vector = super.zero
  
  implicit def degenerate(vector: R2.Vector): Vector =
    new Vector(vector.x, vector.y)
  
  override def apply(x: Scalar, y: Scalar): Vector =
    new Vector(x, y)
  
  override def toString: String = "IR2"
}
