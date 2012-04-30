/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

class R4 extends Vector4Space with RealVectorSpace {
  override type Vector = VectorR4
  
  override def apply(coords: Array[Double]): Vector = {
    if (coords.length != 4) throw new DimensionException
    new Vector(coords(0), coords(1), coords(2), coords(3))
  }
  
  override def apply(x: Real, y: Real, z: Real, w: Real): Vector =
    new Vector(x, y, z, w)
  
  override def toString: String = "R4"
}
