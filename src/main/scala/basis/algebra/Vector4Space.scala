/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Vector4Space extends VectorSpace { self =>
  override type Vector <: Vector4 {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override type Scalar <: Ring {
    type Vector = self.Scalar
  }
  
  override def N: Int = 4
  
  override def apply(coords: TraversableOnce[Scalar]): Vector = {
    val xs = coords.toSeq
    if (xs.length != 4) throw new DimensionException
    apply(xs(0), xs(1), xs(2), xs(3))
  }
  
  def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector
  
  def unapply(vector: Vector): Option[(Scalar, Scalar, Scalar, Scalar)] =
    Some(vector.x, vector.y, vector.z, vector.w)
}
