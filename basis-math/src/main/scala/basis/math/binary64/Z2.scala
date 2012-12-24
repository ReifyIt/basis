/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A 2-dimensional 64-bit two's complement integer module. */
object Z2 extends AffineSpace with F2 with ZN {
  final class Value(
      override val x: Scalar,
      override val y: Scalar)
    extends super[AffineSpace].Value
       with super[F2].Value
       with super[ZN].Value {
    
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
    
    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y
  }
  
  override type Point = Value
  
  override type Vector = Value
  
  override val Vector: Z2.type = Z2
  
  override type Scalar = Integer
  
  override val Scalar: Integer.type = Integer
  
  override def dim: Int = 2
  
  override def origin: Vector = zero
  
  override val zero: Vector =
    new Vector(0L, 0L)
  
  override def apply(x: Scalar, y: Scalar): Vector =
    new Vector(x, y)
  
  override def apply(coords: Array[Long]): Vector = {
    if (coords.length != 2) throw new DimensionException
    new Vector(coords(0), coords(1))
  }
  
  override def toString: String = "Z2"
}
