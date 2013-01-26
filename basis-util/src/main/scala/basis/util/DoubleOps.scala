/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.util

/** Extended `Double` operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
final class DoubleOps(x: Double) {
  def isNaN: Boolean = macro DoubleMacros.isNaN
  
  def isInfinite: Boolean = macro DoubleMacros.isInfinite
  
  def abs: Double = macro DoubleMacros.abs
  
  def min(y: Double): Double = macro DoubleMacros.min
  
  def max(y: Double): Double = macro DoubleMacros.max
  
  def sqrt: Double = macro DoubleMacros.sqrt
  
  def toLongBits: Long = macro DoubleMacros.toLongBits
}

private[util] object DoubleMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply(c: Context): c.Expr[Double] = {
    import c.{Expr, prefix, typeCheck, weakTypeOf}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    Expr[Double](typeCheck(x, weakTypeOf[Double]))
  }
  
  def isNaN(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double"), "isNaN"),
        unApply(c).tree :: Nil))
  }
  
  def isInfinite(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double"), "isInfinite"),
        unApply(c).tree :: Nil))
  }
  
  def abs(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "abs"),
        unApply(c).tree :: Nil))
  }
  
  def min(c: Context)(y: c.Expr[Double]): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "min"),
        unApply(c).tree :: y.tree :: Nil))
  }
  
  def max(c: Context)(y: c.Expr[Double]): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "max"),
        unApply(c).tree :: y.tree :: Nil))
  }
  
  def sqrt(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "sqrt"),
        unApply(c).tree :: Nil))
  }
  
  def toLongBits(c: Context): c.Expr[Long] = {
    import c.Expr
    import c.universe._
    Expr[Long](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double"), "doubleToLongBits"),
        unApply(c).tree :: Nil))
  }
}
