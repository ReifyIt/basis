/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A 3-dimensional real vector.
  * 
  * @author Chris Sachs
  */
final class VectorR3(val x: Real, val y: Real, val z: Real) extends VectorR3.Element {
  override def N: Int = 3
  
  override def apply(i: Int): Real = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def + (that: VectorR3): VectorR3 =
    new VectorR3(x + that.x, y + that.y, z + that.z)
  
  override def unary_- : VectorR3 = new VectorR3(-x, -y, -z)
  
  override def - (that: VectorR3): VectorR3 =
    new VectorR3(x - that.x, y - that.y, z - that.z)
  
  override def :* (scalar: Real): VectorR3 =
    new VectorR3(x * scalar, y * scalar, z * scalar)
  
  override def *: (scalar: Real): VectorR3 = this :* scalar
  
  override def / (scalar: Real): VectorR3 =
    new VectorR3(x / scalar, y / scalar, z / scalar)
  
  override def ⋅ (that: VectorR3): Real =
    x * that.x + y * that.y + z * that.z
  
  override def ⨯ (that: VectorR3): VectorR3 =
    new VectorR3(y * that.z + z * that.y,
                 z * that.x + x * that.z,
                 x * that.y + y * that.x)
  
  override def norm: Real = (x * x + y * y + z * z).sqrt
  
  override def normalized: VectorR3 = this / norm
}

/** A space of 3-dimensional real vectors. */
object VectorR3 extends Vector3Space[R] with RealVectorSpace {
  trait Element extends super[Vector3Space].Element with super[RealVectorSpace].Element
  
  override type Vector = VectorR3
  
  override val zero: VectorR3 = new VectorR3(0.0, 0.0, 0.0)
  
  override def apply(x: Real, y: Real, z: Real): VectorR3 = new VectorR3(x, y, z)
  
  override def apply(coords: Array[Double]): VectorR3 = {
    if (coords.length != 3) throw new DimensionException
    new VectorR3(coords(0), coords(1), coords(2))
  }
  
  override def ⨯ (that: RealVectorSpace): RealMatrixSpace[that.type, this.type] = {
    if (that.isInstanceOf[this.type]) MatrixR3x3.asInstanceOf[RealMatrixSpace[that.type, this.type]]
    else super.⨯(that)
  }
  
  override def toString: String = "R3"
}
