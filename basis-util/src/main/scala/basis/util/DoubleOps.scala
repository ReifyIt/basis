/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.util

/** Supplemental operations on `Double` values.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  */
final class DoubleOps {
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
  
  def isNaN(c: Context): c.Expr[Boolean] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Double = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double")
    Expr(Apply(Select(Double, "isNaN"), x :: Nil))(TypeTag.Boolean)
  }
  
  def isInfinite(c: Context): c.Expr[Boolean] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Double = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double")
    Expr(Apply(Select(Double, "isInfinite"), x :: Nil))(TypeTag.Boolean)
  }
  
  def abs(c: Context): c.Expr[Double] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "abs"), x :: Nil))(TypeTag.Double)
  }
  
  def min(c: Context)(y: c.Expr[Double]): c.Expr[Double] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "min"), x :: y.tree :: Nil))(TypeTag.Double)
  }
  
  def max(c: Context)(y: c.Expr[Double]): c.Expr[Double] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "max"), x :: y.tree :: Nil))(TypeTag.Double)
  }
  
  def sqrt(c: Context): c.Expr[Double] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "sqrt"), x :: Nil))(TypeTag.Double)
  }
  
  def toLongBits(c: Context): c.Expr[Long] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Double = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double")
    Expr(Apply(Select(Double, "doubleToLongBits"), x :: Nil))(TypeTag.Long)
  }
}
