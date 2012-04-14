/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

trait F3 extends FN { self =>
  override type Vector <: VectorF3 {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  final override def dimension: Int = 3
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z, z)
  }
  
  override def apply(coords: TraversableOnce[Scalar]): Vector = {
    val xs = coords.toSeq
    if (xs.length != 3) throw new DimensionException
    apply(xs(0), xs(1), xs(2))
  }
  
  def apply(x: Scalar, y: Scalar, z: Scalar): Vector
}
