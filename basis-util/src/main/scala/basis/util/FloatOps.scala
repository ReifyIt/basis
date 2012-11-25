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

private[util] object FloatMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply(c: Context): c.Expr[Float] = {
    import c.universe._
    val Apply(_, value :: Nil) = c.prefix.tree
    c.Expr(c.typeCheck(value, definitions.FloatTpe))(c.TypeTag.Float)
  }
  
  def abs(c: Context): c.Expr[Float] =
    c.universe.reify(java.lang.Math.abs(unApply(c).splice))
  
  def max(c: Context)(that: c.Expr[Float]): c.Expr[Float] =
    c.universe.reify(java.lang.Math.max(unApply(c).splice, that.splice))
  
  def min(c: Context)(that: c.Expr[Float]): c.Expr[Float] =
    c.universe.reify(java.lang.Math.min(unApply(c).splice, that.splice))
  
  def toIntBits(c: Context): c.Expr[Int] =
    c.universe.reify(java.lang.Float.floatToIntBits(unApply(c).splice))
}
