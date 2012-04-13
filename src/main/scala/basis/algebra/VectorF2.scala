/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait VectorF2 extends VectorFN { self =>
  override type Vector >: self.type <: VectorF2 {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override def Space: F2 {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  final override def dimension: Int = 2
  
  override def + (that: Vector): Vector =
    Space(coord(0) + that.coord(0), coord(1) + that.coord(1))
  
  override def unary_- : Vector =
    Space(-coord(0), -coord(1))
  
  override def - (that: Vector): Vector =
    Space(coord(0) - that.coord(0), coord(1) - that.coord(1))
  
  override def :* (scalar: Scalar): Vector =
    Space(coord(0) * scalar, coord(1) * scalar)
  
  override def *: (scalar: Scalar): Vector =
    Space(scalar * coord(0), scalar * coord(1))
  
  override def ⋅ (that: Vector): Scalar =
    coord(0) * that.coord(0) + coord(1) * that.coord(1)
}
