/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

/** Supplemental operations on `Double` values. */
trait DoubleOps extends Any {
  def abs: Double = macro DoubleMacros.abs
  
  def min(that: Double): Double = macro DoubleMacros.min
  
  def max(that: Double): Double = macro DoubleMacros.max
  
  def sqrt: Double = macro DoubleMacros.sqrt
  
  def toLongBits: Long = macro DoubleMacros.toLongBits
}

private[util] object DoubleMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply(c: Context): c.Expr[Double] = {
    import c.universe._
    val Apply(_, value :: Nil) = c.prefix.tree
    c.Expr(c.typeCheck(value, definitions.DoubleTpe))(c.TypeTag.Double)
  }
  
  def abs(c: Context): c.Expr[Double] =
    c.universe.reify(java.lang.Math.abs(unApply(c).splice))
  
  def max(c: Context)(that: c.Expr[Double]): c.Expr[Double] =
    c.universe.reify(java.lang.Math.max(unApply(c).splice, that.splice))
  
  def min(c: Context)(that: c.Expr[Double]): c.Expr[Double] =
    c.universe.reify(java.lang.Math.min(unApply(c).splice, that.splice))
  
  def sqrt(c: Context): c.Expr[Double] =
    c.universe.reify(java.lang.Math.sqrt(unApply(c).splice))
  
  def toLongBits(c: Context): c.Expr[Long] =
    c.universe.reify(java.lang.Double.doubleToLongBits(unApply(c).splice))
}
