/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A 4-dimensional real vector space.
  * 
  * @author Chris Sachs
  */
object R4 extends F4[Real.type] with RN {
  final class Element(val x: Real, val y: Real, val z: Real, val w: Real)
    extends super[F4].Element with super[RN].Element {
    
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
    
    override def / (scalar: Real): Vector =
      new Vector(x / scalar, y / scalar, z / scalar, w / scalar)
    
    override def ⋅ (that: Vector): Real =
      x * that.x + y * that.y + z * that.z + w * that.w
    
    override def norm: Real = (x * x + y * y + z * z + w * w).sqrt
    
    override def normalized: Vector = this / norm
  }
  
  override type Vector = Element
  
  override def N: Int = 4
  
  override val zero: Vector = new Vector(0.0, 0.0, 0.0, 0.0)
  
  override def apply(x: Real, y: Real, z: Real, w: Real): Vector = new Vector(x, y, z, w)
  
  override def apply(coords: Array[Double]): Vector = {
    if (coords.length != 4) throw new DimensionException
    new Vector(coords(0), coords(1), coords(2), coords(3))
  }
  
  override def map(that: RN): RMxN[this.type, that.type] = {
    if (this eq that) R4x4.asInstanceOf[RMxN[this.type, that.type]]
    else super.map(that)
  }
  
  override def toString: String = "R4"
}
