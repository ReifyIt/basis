/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A 2-dimensional double-precision floating-point vector space. */
object R2 extends AffineSpace with F2 with RN {
  final class Value(
      override val x: Scalar,
      override val y: Scalar)
    extends super[AffineSpace].Value
       with super[F2].Value
       with super[RN].Value {
    
    override def dim: Int = 2
    
    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case _ => throw new java.lang.IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y)
    
    override def unary_- : Vector =
      new Vector(-x, -y)
    
    override def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y)
    
    override def :* (scalar: Scalar): Vector =
      new Vector(x * scalar, y * scalar)
    
    override def *: (scalar: Scalar): Vector = this :* scalar
    
    override def / (scalar: Scalar): Vector =
      new Vector(x / scalar, y / scalar)
    
    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y
    
    override def norm: Scalar =
      (x * x + y * y).sqrt
    
    override def normalized: Vector = this / norm
  }
  
  override type Point = Value
  
  override type Vector = Value
  
  override val Vector: R2.type = R2
  
  override type Scalar = Real
  
  override val Scalar: Real.type = Real
  
  override def dim: Int = 2
  
  override def origin: Vector = zero
  
  override val zero: Vector =
    new Vector(0.0, 0.0)
  
  override def apply(x: Scalar, y: Scalar): Vector =
    new Vector(x, y)
  
  override def apply(coords: Array[Double]): Vector = {
    if (coords.length != 2) throw new DimensionException
    new Vector(coords(0), coords(1))
  }
  
  override def toString: String = "R2"
}
