/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A 2-dimensional real vector.
  * 
  * @author Chris Sachs
  */
final class VectorR2(val x: Real, val y: Real) extends VectorR2.Element {
  override def N: Int = 2
  
  override def apply(i: Int): Real = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def + (that: VectorR2): VectorR2 =
    new VectorR2(x + that.x, y + that.y)
  
  override def unary_- : VectorR2 = new VectorR2(-x, -y)
  
  override def - (that: VectorR2): VectorR2 =
    new VectorR2(x - that.x, y - that.y)
  
  override def :* (scalar: Real): VectorR2 =
    new VectorR2(x * scalar, y * scalar)
  
  override def *: (scalar: Real): VectorR2 = this :* scalar
  
  override def / (scalar: Real): VectorR2 =
    new VectorR2(x / scalar, y / scalar)
  
  override def ⋅ (that: VectorR2): Real =
    x * that.x + y * that.y
  
  override def norm: Real = (x * x + y * y).sqrt
  
  override def normalized: VectorR2 = this / norm
}

/** A space of 2-dimensional real vectors. */
object VectorR2 extends Vector2Space[R] with RealVectorSpace {
  trait Element extends super[Vector2Space].Element with super[RealVectorSpace].Element
  
  override type Vector = VectorR2
  
  override val zero: VectorR2 = new VectorR2(0.0, 0.0)
  
  override def apply(x: Real, y: Real): VectorR2 = new VectorR2(x, y)
  
  override def apply(coords: Array[Double]): VectorR2 = {
    if (coords.length != 2) throw new DimensionException
    new VectorR2(coords(0), coords(1))
  }
  
  override def ⨯ (that: RealVectorSpace): RealMatrixSpace[that.type, this.type] = {
    if (that.isInstanceOf[this.type]) MatrixR2x2.asInstanceOf[RealMatrixSpace[that.type, this.type]]
    else super.⨯(that)
  }
  
  override def toString: String = "R2"
}
