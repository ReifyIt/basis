/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

object R4 extends Vector4Space[Real.type] with RealVectorSpace {
  final class Element(val x: Real, val y: Real, val z: Real, val w: Real)
    extends super[Vector4Space].Element
      with super[RealVectorSpace].Element {
    
    override def N: Int = 4
    
    override def apply(i: Int): Real = i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y, z + that.z, w + that.w)
    
    override def unary_- : Vector = new Vector(-x, -y, -z, -w)
    
    override def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y, z - that.z, w - that.w)
    
    override def :* (scalar: Real): Vector =
      new Vector(x * scalar, y * scalar, z * scalar, w * scalar)
    
    override def *: (scalar: Real): Vector = this :* scalar
    
    override def ⋅ (that: Vector): Real =
      x * that.x + y * that.y + z * that.z + w * that.w
  }
  
  override type Vector = Element
  
  override lazy val zero: Vector = new Vector(0.0, 0.0, 0.0, 0.0)
  
  override def apply(coords: Array[Double]): Vector = {
    if (coords.length != 4) throw new DimensionException
    new Vector(coords(0), coords(1), coords(2), coords(3))
  }
  
  override def apply(x: Real, y: Real, z: Real, w: Real): Vector =
    new Vector(x, y, z, w)
  
  override def toString: String = "R4"
}
