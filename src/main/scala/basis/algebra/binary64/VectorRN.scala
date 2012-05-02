/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class VectorRN private (val Vector: VectorRN.Space, coords: Array[Double])
  extends RealVector.Template {
  
  if (coords.length != Vector.N) throw new DimensionException
  
  override type Vector = VectorRN
  
  override def N: Int = coords.length
  
  override def apply(i: Int): Real = coords(i)
}

object VectorRN {
  def apply(N: Int) = new Space(N)
  
  class Space(val N: Int) extends RealField.Scalar with Affine.Space with RealVector.Space {
    override type Point  = Vector
    override type Vector = VectorRN
    
    override lazy val zero: Vector = new Vector(this, new Array[Double](N))
    
    override def apply(coords: TraversableOnce[Real]): Vector =
      new Vector(this, coords.map(_.toDouble).toArray[Double])
    
    override def apply(coords: Array[Double]): Vector =
      new Vector(this, coords)
    
    def apply(coords: Double*): Vector =
      new Vector(this, coords.toArray[Double])
    
    override def toString: String = "R"+"("+ N + ")"
  }
}
