/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

import language.existentials

class FN[S <: Ring with Singleton](val Scalar: S)(val N: Int) extends VectorSpace[S] {
  final class Element private[FN] (coords: Array[AnyRef]) extends super.Element {
    if (coords.length != Vector.N) throw new DimensionException
    
    override def N: Int = coords.length
    
    override def apply(i: Int): Scalar = coords(i).asInstanceOf[Scalar]
    
    def / [E <: F#Element forSome { type F <: Field { type Value = Scalar } }]
        (scalar: Scalar)(implicit isField: Scalar <:< E): Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (this(i) / scalar).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    def norm[E <: F#Element forSome { type F <: CompleteField { type Value = Scalar } }]
        (implicit isCompleteField: Scalar <:< E): Scalar =
      (this ⋅ this).sqrt
    
    def normalized[E <: F#Element forSome { type F <: CompleteField { type Value = Scalar } }]
        (implicit isCompleteField: Scalar <:< E): Vector =
      this / norm
  }
  
  override type Vector = Element
  
  override lazy val zero: Vector = super.zero
  
  def apply(coords: Scalar*): Vector = new Vector(coords.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
  
  override def toString: String = "FN"+"("+ Scalar +")"+"("+ N +")"
}
