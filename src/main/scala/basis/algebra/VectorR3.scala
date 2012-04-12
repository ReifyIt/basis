/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait VectorR3[V <: VectorR3[V]] extends VectorF3[V, Real] with VectorRN[V] {
  def Space: R3 {
    type Vector = V
  }
  
  override def + (that: V): V =
    Space(this(0) + that(0), this(1) + that(1), this(2) + that(2))
  
  override def unary_- : V =
    Space(-this(0), -this(1), -this(2))
  
  override def - (that: V): V =
    Space(this(0) - that(0), this(1) - that(1), this(2) - that(2))
  
  override def :* (scalar: Double): V =
    Space(this(0) * scalar, this(1) * scalar, this(2) * scalar)
  
  override def *: (scalar: Double): V = this :* scalar
  
  override def ⋅ (that: V): Real =
    new Real(this(0) * that(0) + this(1) * that(1) + this(2) * that(2))
}
