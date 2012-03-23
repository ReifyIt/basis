/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Ring[Self] extends LinearVector[Self, Self] {
  def + (n: Int): Self
  
  def - (n: Int): Self
  
  def * (that: Self): Self
  
  def * (n: Int): Self
  
  def pow(n: Int): Self
  
  def :* (that: Self): Self = this * that
  
  def *: (that: Self): Self = this * that
}
