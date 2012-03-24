/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Ordered[Ordered] {
  def < (that: Ordered): Boolean
  
  def <= (that: Ordered): Boolean
  
  def >= (that: Ordered): Boolean
  
  def > (that: Ordered): Boolean
  
  def min(that: Ordered): Ordered
  
  def max(that: Ordered): Ordered
}
