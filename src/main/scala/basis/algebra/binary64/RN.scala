/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait RN extends LinearSpace with FN { self =>
  override type Vector <: VectorRN {
    type Vector = self.Vector
  }
  
  override type Scalar = Real
  
  override def Scalar = Real
  
  override def zero: Vector = apply(new Array[Double](dimension))
  
  override def apply(coords: Seq[Scalar]): Vector = {
    if (coords.length != dimension) throw new DimensionException
    val xs = new Array[Double](dimension)
    var i = 0
    while (i < xs.length) {
      xs(i) = coords(i).toDouble
      i += 1
    }
    apply(xs)
  }
  
  def apply(coords: Array[Double]): Vector
}
