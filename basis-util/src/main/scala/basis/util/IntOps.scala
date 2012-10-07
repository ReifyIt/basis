/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis
package util

trait IntOps {
  import scala.language.experimental.macros
  
  def abs: Int = macro IntMacros.abs
  
  def max(that: Int): Int = macro IntMacros.max
  
  def min(that: Int): Int = macro IntMacros.min
  
  def to(end: Int): Range = macro IntMacros.to
  
  def until(end: Int): Range = macro IntMacros.until
}
