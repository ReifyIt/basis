/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait F2 extends CoordinateSpace {
  type Vector <: VectorF2[Vector, Scalar]
  
  final def dimension: Int = 2
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z)
  }
  
  def apply(coords: Seq[Scalar]): Vector = {
    if (coords.length != 2) throw new DimensionException
    apply(coords(0), coords(1))
  }
  
  def apply(x: Scalar, y: Scalar): Vector
}
