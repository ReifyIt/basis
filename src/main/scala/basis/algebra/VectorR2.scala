/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait VectorR2 extends VectorF2 with VectorRN { self =>
  override type Vector >: self.type <: VectorR2 {
    type Vector = self.Vector
  }
  
  override def Space: R2 {
    type Vector = self.Vector
  }
  
  override def + (that: Vector): Vector =
    Space(this(0) + that(0), this(1) + that(1))
  
  override def unary_- : Vector =
    Space(-this(0), -this(1))
  
  override def - (that: Vector): Vector =
    Space(this(0) - that(0), this(1) - that(1))
  
  override def :* (scalar: Double): Vector =
    Space(this(0) * scalar, this(1) * scalar)
  
  override def *: (scalar: Double): Vector = this :* scalar
  
  override def ⋅ (that: Vector): Real =
    new Real(this(0) * that(0) + this(1) * that(1))
}
