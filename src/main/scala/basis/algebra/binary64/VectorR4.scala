/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A 4-dimensional real vector.
  * 
  * @author Chris Sachs
  */
final class VectorR4(val x: Real, val y: Real, val z: Real, val w: Real) extends VectorR4.Element {
  override def N: Int = 4
  
  override def apply(i: Int): Real = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case 3 => w
    case _ => throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def + (that: VectorR4): VectorR4 =
    new VectorR4(x + that.x, y + that.y, z + that.z, w + that.w)
  
  override def unary_- : VectorR4 = new VectorR4(-x, -y, -z, -w)
  
  override def - (that: VectorR4): VectorR4 =
    new VectorR4(x - that.x, y - that.y, z - that.z, w - that.w)
  
  override def :* (scalar: Real): VectorR4 =
    new VectorR4(x * scalar, y * scalar, z * scalar, w * scalar)
  
  override def *: (scalar: Real): VectorR4 = this :* scalar
  
  override def / (scalar: Real): VectorR4 =
    new VectorR4(x / scalar, y / scalar, z / scalar, w / scalar)
  
  override def ⋅ (that: VectorR4): Real =
    x * that.x + y * that.y + z * that.z + w * that.w
  
  override def norm: Real = (x * x + y * y + z * z + w * w).sqrt
  
  override def normalized: VectorR4 = this / norm
}

/** A space of 4-dimensional real vectors. */
object VectorR4 extends Vector4Space[R] with RealVectorSpace {
  trait Element extends super[Vector4Space].Element with super[RealVectorSpace].Element
  
  override type Vector = VectorR4
  
  override val zero: VectorR4 = new VectorR4(0.0, 0.0, 0.0, 0.0)
  
  override def apply(x: Real, y: Real, z: Real, w: Real): VectorR4 = new VectorR4(x, y, z, w)
  
  override def apply(coords: Array[Double]): VectorR4 = {
    if (coords.length != 4) throw new DimensionException
    new VectorR4(coords(0), coords(1), coords(2), coords(3))
  }
  
  override def ⨯ (that: RealVectorSpace): RealMatrixSpace[that.type, this.type] = {
    if (that.isInstanceOf[this.type]) MatrixR4x4.asInstanceOf[RealMatrixSpace[that.type, this.type]]
    else super.⨯(that)
  }
  
  override def toString: String = "R4"
}
