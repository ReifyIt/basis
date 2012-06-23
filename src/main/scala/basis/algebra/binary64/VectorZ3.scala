/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A 3-dimensional integer vector.
  * 
  * @author Chris Sachs
  */
final class VectorZ3(val x: Integer, val y: Integer, val z: Integer) extends VectorZ3.Element {
  override def N: Int = 3
  
  override def apply(i: Int): Integer = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def + (that: VectorZ3): VectorZ3 =
    new VectorZ3(x + that.x, y + that.y, z + that.z)
  
  override def unary_- : VectorZ3 = new VectorZ3(-x, -y, -z)
  
  override def - (that: VectorZ3): VectorZ3 =
    new VectorZ3(x - that.x, y - that.y, z - that.z)
  
  override def :* (scalar: Integer): VectorZ3 =
    new VectorZ3(x * scalar, y * scalar, z * scalar)
  
  override def *: (scalar: Integer): VectorZ3 = this :* scalar
  
  override def ⋅ (that: VectorZ3): Integer =
    x * that.x + y * that.y + z * that.z
  
  override def ⨯ (that: VectorZ3): VectorZ3 =
    new VectorZ3(y * that.z + z * that.y,
                 z * that.x + x * that.z,
                 x * that.y + y * that.x)
}

/** A module of 3-dimensional integer vectors. */
object VectorZ3 extends Vector3Space[Z] with IntegerModule {
  trait Element extends super[Vector3Space].Element with super[IntegerModule].Element
  
  override type Vector = VectorZ3
  
  override lazy val zero: VectorZ3 = new VectorZ3(0L, 0L, 0L)
  
  override def apply(x: Integer, y: Integer, z: Integer): VectorZ3 = new VectorZ3(x, y, z)
  
  override def apply(coords: Array[Long]): VectorZ3 = {
    if (coords.length != 3) throw new DimensionException
    new VectorZ3(coords(0), coords(1), coords(2))
  }
  
  override def toString: String = "Z3"
}
