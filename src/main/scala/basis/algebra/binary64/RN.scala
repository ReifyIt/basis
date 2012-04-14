/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import generic._

trait RN extends LinearSpace with FN { self =>
  override type Vector <: VectorRN {
    type Vector = self.Vector
  }
  
  override type Scalar = Real
  
  override def Scalar = Real
  
  override def zero: Vector = apply(new Array[Double](dimension))
  
  override def apply(coords: TraversableOnce[Scalar]): Vector = {
    val xs = coords.toSeq
    if (xs.length != dimension) throw new DimensionException
    // apply(xs.map(_.toDouble).toArray[Double]) // I know.
    val array = new Array[Double](dimension)
    var i = 0
    while (i < array.length) {
      array(i) = xs(i).toDouble
      i += 1
    }
    apply(array)
  }
  
  def apply(coords: Array[Double]): Vector
}
