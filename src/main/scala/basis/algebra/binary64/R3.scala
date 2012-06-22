/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A concrete 3-dimensional coordinate space over the binary64 `Real` field.
  * 
  * @author Chris Sachs
  * 
  * @define Structure   `R3` space
  */
object R3 extends Vector3Space[Real.type] with RealVectorSpace {
  /** A vector element of this $Structure. */
  final class Element(val x: Real, val y: Real, val z: Real)
    extends super[Vector3Space].Element
      with super[RealVectorSpace].Element {
    
    override def N: Int = 3
    
    override def apply(i: Int): Real = i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y, z + that.z)
    
    override def unary_- : Vector = new Vector(-x, -y, -z)
    
    override def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y, z - that.z)
    
    override def :* (scalar: Real): Vector =
      new Vector(x * scalar, y * scalar, z * scalar)
    
    override def *: (scalar: Real): Vector = this :* scalar
    
    override def / (scalar: Real): Vector =
      new Vector(x / scalar, y / scalar, z / scalar)
    
    override def ⋅ (that: Vector): Real =
      x * that.x + y * that.y + z * that.z
    
    override def ⨯ (that: Vector): Vector =
      new Vector(y * that.z + z * that.y,
                 z * that.x + x * that.z,
                 x * that.y + y * that.x)
    
    override def norm: Real = (x * x + y * y + z * z).sqrt
  }
  
  override type Vector = Element
  
  override lazy val zero: Vector = new Vector(0.0, 0.0, 0.0)
  
  override def apply(coords: Array[Double]): Vector = {
    if (coords.length != 3) throw new DimensionException
    new Vector(coords(0), coords(1), coords(2))
  }
  
  override def apply(x: Real, y: Real, z: Real): Vector = new Vector(x, y, z)
  
  override def ⨯ (that: RealVectorSpace): RealMatrixSpace[that.type, this.type] = {
    if (that.isInstanceOf[R3.type]) R3x3.asInstanceOf[RealMatrixSpace[that.type, this.type]]
    else super.⨯(that)
  }
  
  override def toString: String = "R3"
}
