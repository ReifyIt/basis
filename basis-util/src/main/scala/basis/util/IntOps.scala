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

private[util] object IntMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, value :: Nil) = c.prefix.tree
    c.Expr(c.typeCheck(value, definitions.IntTpe))(c.TypeTag.Int)
  }
  
  def abs(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Math.abs(unApply(c).splice))
  
  def max(c: Context)(that: c.Expr[Int]): c.Expr[Int] =
    c.universe.reify(java.lang.Math.max(unApply(c).splice, that.splice))
  
  def min(c: Context)(that: c.Expr[Int]): c.Expr[Int] =
    c.universe.reify(java.lang.Math.min(unApply(c).splice, that.splice))
  
  def signum(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Integer.signum(unApply(c).splice))
  
  def countSetBits(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Integer.bitCount(unApply(c).splice))
  
  def countLeadingZeros(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Integer.numberOfLeadingZeros(unApply(c).splice))
  
  def countTrailingZeros(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Integer.numberOfTrailingZeros(unApply(c).splice))
  
  def toFloatBits(c: Context): c.Expr[Float] =
    c.universe.reify(java.lang.Float.intBitsToFloat(unApply(c).splice))
}
