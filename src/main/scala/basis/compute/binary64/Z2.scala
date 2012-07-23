/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.compute
package binary64

import basis.number._

/** A 2-dimensional integer module.
  * 
  * @author Chris Sachs
  */
object Z2 extends F2[Int64.type] with ZN {
  final class Element(val x: Scalar, val y: Scalar)
    extends super[F2].Element with super[ZN].Element {
    
    override def N: Int = 2
    
    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y)
    
    override def unary_- : Vector = new Vector(-x, -y)
    
    override def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y)
    
    override def :* (scalar: Scalar): Vector =
      new Vector(x * scalar, y * scalar)
    
    override def *: (scalar: Scalar): Vector = this :* scalar
    
    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y
  }
  
  override type Vector = Element
  
  override def N: Int = 2
  
  override val zero: Vector = new Vector(0L, 0L)
  
  override def apply(x: Scalar, y: Scalar): Vector = new Vector(x, y)
  
  override def apply(coords: Array[Long]): Vector = {
    if (coords.length != 2) throw new DimensionException
    new Vector(coords(0), coords(1))
  }
  
  override def toString: String = "Z2"
}
