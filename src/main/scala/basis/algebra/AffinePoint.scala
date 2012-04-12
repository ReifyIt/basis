/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait AffinePoint[P <: AffinePoint[P, V, S],
                  V <: LinearVector[V, S],
                  S <: Ring[S]] {
  
  def Space: AffineModule {
    type Point = P
    type Vector = V
    type Scalar = S
  }
  
  def + (vector: V): P
  
  def - (vector: V): P
  
  def - (that: P): V
}
