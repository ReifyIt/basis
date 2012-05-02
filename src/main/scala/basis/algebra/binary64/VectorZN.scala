/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class VectorZN private (val Vector: VectorZN.Space, coords: Array[Long])
  extends IntegerVector.Template {
  
  if (coords.length != Vector.N) throw new DimensionException
  
  override type Vector = VectorZN
  
  override def N: Int = coords.length
  
  override def apply(i: Int): Integer = coords(i)
}

object VectorZN {
  def apply(N: Int): Space = new Space(N)
  
  class Space(val N: Int) extends OrderedRing.Scalar with Affine.Space with IntegerVector.Space {
    override type Point  = Vector
    override type Vector = VectorZN
    
    override lazy val zero: Vector = new Vector(this, new Array[Long](N))
    
    override def apply(coords: TraversableOnce[Integer]): Vector =
      new Vector(this, coords.map(_.toLong).toArray[Long])
    
    override def apply(coords: Array[Long]): Vector =
      new Vector(this, coords)
    
    def apply(coords: Long*): Vector =
      new Vector(this, coords.toArray[Long])
    
    override def toString: String = "Z"+"("+ N + ")"
  }
}
