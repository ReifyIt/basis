/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A 3-dimensional double-precision floating-point vector space.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
object R3 extends AffineSpace with F3 with RN {
  final class Value(
      override val x: Scalar,
      override val y: Scalar,
      override val z: Scalar)
    extends super[AffineSpace].Value
       with super[F3].Value
       with super[RN].Value {
    
    override def dim: Int = 3
    
    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new java.lang.IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y, z + that.z)
    
    override def unary_- : Vector =
      new Vector(-x, -y, -z)
    
    override def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y, z - that.z)
    
    override def :* (scalar: Scalar): Vector =
      new Vector(x * scalar, y * scalar, z * scalar)
    
    override def *: (scalar: Scalar): Vector = this :* scalar
    
    override def / (scalar: Scalar): Vector =
      new Vector(x / scalar, y / scalar, z / scalar)
    
    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y + z * that.z
    
    override def ⨯ (that: Vector): Vector =
      new Vector(y * that.z + z * that.y,
                 z * that.x + x * that.z,
                 x * that.y + y * that.x)
    
    override def norm: Scalar =
      (x * x + y * y + z * z).sqrt
    
    override def normalized: Vector = this / norm
  }
  
  override type Point = Value
  
  override type Vector = Value
  
  override val Vector: R3.type = R3
  
  override type Scalar = Real
  
  override val Scalar: Real.type = Real
  
  override def dim: Int = 3
  
  override def origin: Vector = zero
  
  override val zero: Vector =
    new Vector(0.0, 0.0, 0.0)
  
  override def apply(x: Scalar, y: Scalar, z: Scalar): Vector =
    new Vector(x, y, z)
  
  override def apply(coords: Array[Double]): Vector = {
    if (coords.length != 3) throw new DimensionException
    new Vector(coords(0), coords(1), coords(2))
  }
  
  override def toString: String = "R3"
}
