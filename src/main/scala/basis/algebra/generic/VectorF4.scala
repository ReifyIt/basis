/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

trait VectorF4 extends VectorFN { self =>
  override type Space <: F4 with Singleton {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override type Vector >: self.type <: VectorF4 {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  final override def dimension: Int = 4
  
  override def + (that: Vector): Vector =
    Space(coord(0) + that.coord(0), coord(1) + that.coord(1), coord(2) + that.coord(2), coord(3) + that.coord(3))
  
  override def unary_- : Vector =
    Space(-coord(0), -coord(1), -coord(2), -coord(3))
  
  override def - (that: Vector): Vector =
    Space(coord(0) - that.coord(0), coord(1) - that.coord(1), coord(2) - that.coord(2), coord(3) - that.coord(3))
  
  override def :* (scalar: Scalar): Vector =
    Space(coord(0) * scalar, coord(1) * scalar, coord(2) * scalar, coord(3) * scalar)
  
  override def *: (scalar: Scalar): Vector =
    Space(scalar * coord(0), scalar * coord(1), scalar * coord(2), scalar * coord(3))
  
  override def ⋅ (that: Vector): Scalar =
    coord(0) * that.coord(0) + coord(1) * that.coord(1) + coord(2) * that.coord(2) + coord(3) * that.coord(3)
}
