/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A 2-dimensional integer vector.
  * 
  * @author Chris Sachs
  */
final class VectorZ2(val x: Integer, val y: Integer) extends VectorZ2.Element {
  override def N: Int = 2
  
  override def apply(i: Int): Integer = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def + (that: VectorZ2): VectorZ2 =
    new VectorZ2(x + that.x, y + that.y)
  
  override def unary_- : VectorZ2 = new VectorZ2(-x, -y)
  
  override def - (that: VectorZ2): VectorZ2 =
    new VectorZ2(x - that.x, y - that.y)
  
  override def :* (scalar: Integer): VectorZ2 =
    new VectorZ2(x * scalar, y * scalar)
  
  override def *: (scalar: Integer): VectorZ2 = this :* scalar
  
  override def ⋅ (that: VectorZ2): Integer =
    x * that.x + y * that.y
}

/** A module of 2-dimensional integer vectors. */
object VectorZ2 extends Vector2Space[Z] with IntegerModule {
  trait Element extends super[Vector2Space].Element with super[IntegerModule].Element
  
  override type Vector = VectorZ2
  
  override val zero: VectorZ2 = new VectorZ2(0L, 0L)
  
  override def apply(x: Integer, y: Integer): VectorZ2 = new VectorZ2(x, y)
  
  override def apply(coords: Array[Long]): VectorZ2 = {
    if (coords.length != 2) throw new DimensionException
    new VectorZ2(coords(0), coords(1))
  }
  
  override def toString: String = "Z2"
}
