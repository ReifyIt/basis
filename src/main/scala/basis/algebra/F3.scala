/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait F3 extends VectorSpace with FN {
  type Vector <: VectorF3[Vector, Scalar]
  
  final def dimension: Int = 3
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z, z)
  }
  
  def apply(coords: Seq[Scalar]): Vector = {
    if (coords.length != 3) throw new DimensionException
    apply(coords(0), coords(1), coords(2))
  }
  
  def apply(x: Scalar, y: Scalar, z: Scalar): Vector
}
