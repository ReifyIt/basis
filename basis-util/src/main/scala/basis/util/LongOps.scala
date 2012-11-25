/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

/** Supplemental operations on `Long` values. */
trait LongOps extends Any {
  def abs: Long = macro LongMacros.abs
  
  def min(that: Long): Long = macro LongMacros.min
  
  def max(that: Long): Long = macro LongMacros.max
  
  def signum: Int = macro LongMacros.signum
  
  def countSetBits: Int = macro LongMacros.countSetBits
  
  def countLeadingZeros: Int = macro LongMacros.countLeadingZeros
  
  def countTrailingZeros: Int = macro LongMacros.countTrailingZeros
  
  def toDoubleBits: Double = macro LongMacros.toDoubleBits
}

private[util] object LongMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply(c: Context): c.Expr[Long] = {
    import c.universe._
    val Apply(_, value :: Nil) = c.prefix.tree
    c.Expr(c.typeCheck(value, definitions.LongTpe))(c.TypeTag.Long)
  }
  
  def abs(c: Context): c.Expr[Long] =
    c.universe.reify(java.lang.Math.abs(unApply(c).splice))
  
  def max(c: Context)(that: c.Expr[Long]): c.Expr[Long] =
    c.universe.reify(java.lang.Math.max(unApply(c).splice, that.splice))
  
  def min(c: Context)(that: c.Expr[Long]): c.Expr[Long] =
    c.universe.reify(java.lang.Math.min(unApply(c).splice, that.splice))
  
  def signum(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Long.signum(unApply(c).splice))
  
  def countSetBits(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Long.bitCount(unApply(c).splice))
  
  def countLeadingZeros(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Long.numberOfLeadingZeros(unApply(c).splice))
  
  def countTrailingZeros(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Long.numberOfTrailingZeros(unApply(c).splice))
  
  def toDoubleBits(c: Context): c.Expr[Double] =
    c.universe.reify(java.lang.Double.longBitsToDouble(unApply(c).splice))
}
