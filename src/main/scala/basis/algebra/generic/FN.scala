/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

import language.existentials

/** A generic ''N''-dimensional coordinate space over a commutative ring.
  * 
  * @author Chris Sachs
  * 
  * @tparam S   The singleton type of the scalar structure of this $Structure.
  * 
  * @define Structure   `FN` space
  */
class FN[S <: Ring with Singleton](val Scalar: S)(val N: Int) extends VectorSpace[S] {
  /** A vector element of this $Structure. */
  final class Element private[FN] (coords: Array[AnyRef]) extends super.Element {
    if (coords.length != Vector.N) throw new DimensionException
    
    override def N: Int = coords.length
    
    override def apply(i: Int): Scalar = coords(i).asInstanceOf[Scalar]
    
    /** Returns the quotient of this $vector divided by a $scalar.
      * 
      * @usecase def / (scalar: Scalar): Vector
      *   @inheritdoc
      */
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
    
    /** Returns the Euclidean norm of this $vector.
      * 
      * @usecase def norm: Scalar
      *   @inheritdoc
      */
    def norm[E <: F#Element forSome { type F <: CompleteField { type Value = Scalar } }]
        (implicit isCompleteField: Scalar <:< E): Scalar =
      (this ⋅ this).sqrt
    
    /** Returns a $vector in the same direction as this $vector but scaled to unit length.
      * 
      * @usecase def normalized: Vector
      *   @inheritdoc
      */
    def normalized[E <: F#Element forSome { type F <: CompleteField { type Value = Scalar } }]
        (implicit isCompleteField: Scalar <:< E): Vector =
      this / norm
  }
  
  override type Vector = Element
  
  override lazy val zero: Vector = super.zero
  
  override def apply(coords: Scalar*): Vector = new Vector(coords.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
  
  override def toString: String = "FN"+"("+ Scalar +")"+"("+ N +")"
}
