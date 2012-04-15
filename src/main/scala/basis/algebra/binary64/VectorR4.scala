/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import generic._

trait VectorR4 extends VectorF4 with VectorRN { self =>
  override type Space <: R4 with Singleton {
    type Vector = self.Vector
  }
  
  override type Vector >: self.type <: VectorR4 {
    type Vector = self.Vector
  }
  
  override def + (that: Vector): Vector =
    Space(this(0) + that(0), this(1) + that(1), this(2) + that(2), this(3) + that(3))
  
  override def unary_- : Vector =
    Space(-this(0), -this(1), -this(2), -this(3))
  
  override def - (that: Vector): Vector =
    Space(this(0) - that(0), this(1) - that(1), this(2) - that(2), this(3) - that(3))
  
  override def :* (scalar: Double): Vector =
    Space(this(0) * scalar, this(1) * scalar, this(2) * scalar, this(3) * scalar)
  
  override def *: (scalar: Double): Vector = this :* scalar
  
  override def ⋅ (that: Vector): Real =
    new Real(this(0) * that(0) + this(1) * that(1) + this(2) * that(2) + this(3) * that(3))
}
