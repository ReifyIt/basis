/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

/** Supplemental operations on `Int` values. */
trait IntOps extends Any {
  def abs: Int = macro IntMacros.abs
  
  def min(that: Int): Int = macro IntMacros.min
  
  def max(that: Int): Int = macro IntMacros.max
  
  def signum: Int = macro IntMacros.signum
  
  def countSetBits: Int = macro IntMacros.countSetBits
  
  def countLeadingZeros: Int = macro IntMacros.countLeadingZeros
  
  def countTrailingZeros: Int = macro IntMacros.countTrailingZeros
  
  def toFloatBits: Float = macro IntMacros.toFloatBits
}
