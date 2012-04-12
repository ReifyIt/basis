/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait VectorF2[V <: VectorF2[V, S], S <: Ring[S]] extends VectorFN[V, S] {
  def Space: F2 {
    type Vector = V
    type Scalar = S
  }
  
  final override def dimension: Int = 2
  
  override def + (that: V): V =
    Space(coord(0) + that.coord(0), coord(1) + that.coord(1))
  
  override def unary_- : V =
    Space(-coord(0), -coord(1))
  
  override def - (that: V): V =
    Space(coord(0) - that.coord(0), coord(1) - that.coord(1))
  
  override def :* (scalar: S): V =
    Space(coord(0) * scalar, coord(1) * scalar)
  
  override def *: (scalar: S): V =
    Space(scalar * coord(0), scalar * coord(1))
  
  override def ⋅ (that: V): S =
    coord(0) * that.coord(0) + coord(1) * that.coord(1)
}
