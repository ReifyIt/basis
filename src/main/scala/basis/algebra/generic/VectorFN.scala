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
  def apply[F <: Ring { type Vector = F }]
      (Scalar: Ring.Space { type Vector = F })(N: Int): Space[F] =
    new Space[F](Scalar)(N)
  
  class Space[F <: Ring { type Vector = F }]
      (val Scalar: Ring.Space { type Vector = F })
      (val N: Int)
    extends Ring.Scalar with Affine.Space with Vector.Space {
    
    override type Point  = Vector
    override type Vector = VectorFN[F]
    override type Scalar = F
    
    lazy val zero: Vector = {
      val z = Scalar.zero.asInstanceOf[AnyRef]
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = z
        i += 1
      }
      apply(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def apply(coords: TraversableOnce[Scalar]): Vector =
      new Vector(this, coords.asInstanceOf[TraversableOnce[AnyRef]].toArray[AnyRef])
    
    def apply(coords: Scalar*): Vector =
      new Vector(this, coords.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
    
    override def toString: String = "F"+"("+ Scalar +")"+"("+ N +")"
  }
}
