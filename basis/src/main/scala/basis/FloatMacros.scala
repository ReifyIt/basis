/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

private[basis] object FloatMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def MinValue(c: Context): c.Expr[Float] = c.literal(scala.Float.MinValue)
  
  def MaxValue(c: Context): c.Expr[Float] = c.literal(scala.Float.MaxValue)
  
  def Infinity(c: Context): c.Expr[Float] = c.literal(scala.Float.PositiveInfinity)
  
  def NaN(c: Context): c.Expr[Float] = c.literal(scala.Float.NaN)
  
  def abs(c: Context): c.Expr[Float] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "abs"), self :: Nil))(WeakTypeTag.Float)
  }
  
  def min(c: Context)(that: c.Expr[Float]): c.Expr[Float] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "min"), self :: that.tree :: Nil))(WeakTypeTag.Float)
  }
  
  def max(c: Context)(that: c.Expr[Float]): c.Expr[Float] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "max"), self :: that.tree :: Nil))(WeakTypeTag.Float)
  }
  
  def toIntBits(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Float = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Float")
    c.Expr(Apply(Select(Float, "floatToIntBits"), self :: Nil))(WeakTypeTag.Int)
  }
}
