/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait GeneralVector[V, S] {
  def Space: FreeModule {
    type Vector = V
    type Scalar = S
  }
  
  def + (that: V): V
  
  def unary_- : V
  
  def - (that: V): V
  
  def :* (scalar: S): V
  
  def *: (scalar: S): V
}