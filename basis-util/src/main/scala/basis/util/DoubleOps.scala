/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

/** Supplemental operations on `Double` values. */
abstract class DoubleOps private[util] {
  def abs: Double = macro DoubleMacros.abs
  
  def min(that: Double): Double = macro DoubleMacros.min
  
  def max(that: Double): Double = macro DoubleMacros.max
  
  def sqrt: Double = macro DoubleMacros.sqrt
  
  def toLongBits: Long = macro DoubleMacros.toLongBits
}
