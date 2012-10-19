/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

/** Supplemental operations on `Float` values. */
trait FloatOps extends Any {
  def abs: Float = macro FloatMacros.abs
  
  def min(that: Float): Float = macro FloatMacros.min
  
  def max(that: Float): Float = macro FloatMacros.max
  
  def toIntBits: Int = macro FloatMacros.toIntBits
}
