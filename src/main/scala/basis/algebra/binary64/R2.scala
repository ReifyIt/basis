/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

class R2 extends Vector2Space with RealVectorSpace {
  override type Vector = VectorR2
  
  override def apply(coords: Array[Double]): Vector = {
    if (coords.length != 2) throw new DimensionException
    new Vector(coords(0), coords(1))
  }
  
  override def apply(x: Real, y: Real): Vector =
    new Vector(x, y)
  
  override def toString: String = "R2"
}
