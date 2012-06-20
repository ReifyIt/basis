/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package decimal

final class Real(
    override val significand: Integer,
    override val error: Integer,
    override val exponent: Int)
  extends Real.Element

object Real extends binary.FloatingPoint {
  override type Vector = Real
  
  override def radix: Int = 10
  
  override def precision: Int = 128
  
  override def apply(significand: Integer, error: Integer, exponent: Int): Real =
    new Real(significand, error, exponent)
  
  override def toString: String = "Real"
}
