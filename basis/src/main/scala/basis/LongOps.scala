/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

trait LongOps extends Any {
  import scala.language.experimental.macros
  
  def abs: Long = macro LongMacros.abs
  
  def min(that: Long): Long = macro LongMacros.min
  
  def max(that: Long): Long = macro LongMacros.max
  
  def signum: Int = macro LongMacros.signum
  
  def countSetBits: Int = macro LongMacros.countSetBits
  
  def countLeadingZeros: Int = macro LongMacros.countLeadingZeros
  
  def countTrailingZeros: Int = macro LongMacros.countTrailingZeros
  
  def toDoubleBits: Double = macro LongMacros.toDoubleBits
}
