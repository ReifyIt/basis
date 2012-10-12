/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

private[basis] object DoubleMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def MinValue(c: Context): c.Expr[Double] = c.literal(scala.Double.MinValue)
  
  def MaxValue(c: Context): c.Expr[Double] = c.literal(scala.Double.MaxValue)
  
  def Infinity(c: Context): c.Expr[Double] = c.literal(scala.Double.PositiveInfinity)
  
  def NaN(c: Context): c.Expr[Double] = c.literal(scala.Double.NaN)
  
  def abs(c: Context): c.Expr[Double] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "abs"), self :: Nil))(WeakTypeTag.Double)
  }
  
  def min(c: Context)(that: c.Expr[Double]): c.Expr[Double] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "min"), self :: that.tree :: Nil))(WeakTypeTag.Double)
  }
  
  def max(c: Context)(that: c.Expr[Double]): c.Expr[Double] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "max"), self :: that.tree :: Nil))(WeakTypeTag.Double)
  }
  
  def sqrt(c: Context): c.Expr[Double] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "sqrt"), self :: Nil))(WeakTypeTag.Double)
  }
  
  def toLongBits(c: Context): c.Expr[Long] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Double = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double")
    c.Expr(Apply(Select(Double, "doubleToLongBits"), self :: Nil))(WeakTypeTag.Long)
  }
}
