/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait F4 extends CoordinateSpace {
  type Vector <: VectorF4[Vector, Scalar]
  
  final def dimension: Int = 4
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z, z, z)
  }
  
  def apply(coords: Seq[Scalar]): Vector = {
    if (coords.length != 4) throw new DimensionException
    apply(coords(0), coords(1), coords(2), coords(3))
  }
  
  def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector
}
