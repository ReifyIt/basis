/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class VectorFN[F <: Ring { type Vector = F }] private
    (val Vector: VectorFN.Space[F], coords: Array[AnyRef])
  extends Vector.Template {
  
  if (coords.length != Vector.N) throw new DimensionException
  
  override type Vector = VectorFN[F]
  override type Scalar = F
  
  override def N: Int = coords.length
  
  override def apply(i: Int): Scalar = coords(i).asInstanceOf[Scalar]
}

object VectorFN {
  def apply[F <: Ring { type Vector = F }](N: Int) = new Space[F](N)
  
  class Space[F <: Ring { type Vector = F }](val N: Int) extends Vector.Space {
    override type Vector = VectorFN[F]
    override type Scalar = F
    
    override def apply(coords: TraversableOnce[Scalar]): Vector =
      new Vector(this, coords.asInstanceOf[TraversableOnce[AnyRef]].toArray[AnyRef])
    
    def apply(coords: Scalar*): Vector =
      new Vector(this, coords.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
    
    override def toString: String = "F"+"("+ N + ")"
  }
}
