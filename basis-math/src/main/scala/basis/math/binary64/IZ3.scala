/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A 3-dimensional 64-bit two's complement integer interval module.
  * 
  * @author Chris Sachs
  * @since  0.1
  * @group  Integral
  */
object IZ3 extends AffineSpace with IZN with F3 {
  final class Value(
      override val x: Scalar,
      override val y: Scalar,
      override val z: Scalar)
    extends super[AffineSpace].Value
       with super[IZN].Value
       with super[F3].Value {
    
    def contains(vector: Z3.Vector): Boolean =
      x.contains(vector.x) && y.contains(vector.y) && z.contains(vector.z)
  }
  
  override type Point = Value
  
  override type Vector = Value
  
  override val Vector: IZ3.type = IZ3
  
  override type Scalar = IntegerInterval
  
  override val Scalar: IntegerInterval.type = IntegerInterval
  
  override def origin: Vector = zero
  
  override val zero: Vector = super.zero
  
  implicit def degenerate(vector: Z3.Vector): Vector =
    new Vector(vector.x, vector.y, vector.z)
  
  override def apply(x: Scalar, y: Scalar, z: Scalar): Vector =
    new Vector(x, y, z)
  
  override def toString: String = "IZ3"
}
