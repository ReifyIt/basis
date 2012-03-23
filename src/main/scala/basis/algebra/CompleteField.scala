/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait CompleteField[Self] extends Field[Self] {
  def + (x: Double): Self
  
  def - (x: Double): Self
  
  def * (x: Double): Self
  
  def / (x: Double): Self
  
  def pow(that: Self): Self
  
  def pow(x: Double): Self
  
  def sqrt: Self
}
