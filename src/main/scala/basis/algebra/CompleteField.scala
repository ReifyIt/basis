/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait CompleteField[CompleteField] extends Field[CompleteField] {
  def + (x: Double): CompleteField
  
  def - (x: Double): CompleteField
  
  def * (x: Double): CompleteField
  
  def / (x: Double): CompleteField
  
  def pow(that: CompleteField): CompleteField
  
  def pow(x: Double): CompleteField
  
  def sqrt: CompleteField
}
