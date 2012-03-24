/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Ring[Ring] extends Vector[Ring, Ring] {
  def + (n: Int): Ring
  
  def - (n: Int): Ring
  
  def * (that: Ring): Ring
  
  def * (n: Int): Ring
  
  def pow(n: Int): Ring
  
  def :* (that: Ring): Ring = this * that
  
  def *: (that: Ring): Ring = this * that
}
