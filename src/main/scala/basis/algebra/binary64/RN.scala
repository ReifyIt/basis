/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

class RN(val N: Int) extends RealVectorSpace {
  override type Vector = VectorRN
  
  override def apply(coords: TraversableOnce[Real]): Vector =
    new Vector(this, coords.map(_.toDouble).toArray[Double])
  
  override def apply(coords: Array[Double]): Vector =
    new Vector(this, coords)
  
  def apply(coords: Double*): Vector =
    new Vector(this, coords.toArray[Double])
  
  override def toString: String = "R"+"("+ N + ")"
}
