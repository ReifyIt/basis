/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

private[util] object IntMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def abs(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "abs"), self :: Nil))(WeakTypeTag.Int)
  }
  
  def min(c: Context)(that: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "min"), self :: that.tree :: Nil))(WeakTypeTag.Int)
  }
  
  def max(c: Context)(that: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "max"), self :: that.tree :: Nil))(WeakTypeTag.Int)
  }
  
  def signum(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Integer = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer")
    c.Expr(Apply(Select(Integer, "signum"), self :: Nil))(WeakTypeTag.Int)
  }
  
  def countSetBits(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Integer = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer")
    c.Expr(Apply(Select(Integer, "bitCount"), self :: Nil))(WeakTypeTag.Int)
  }
  
  def countLeadingZeros(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Integer = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer")
    c.Expr(Apply(Select(Integer, "numberOfLeadingZeros"), self :: Nil))(WeakTypeTag.Int)
  }
  
  def countTrailingZeros(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Integer = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer")
    c.Expr(Apply(Select(Integer, "numberOfTrailingZeros"), self :: Nil))(WeakTypeTag.Int)
  }
  
  def toFloatBits(c: Context): c.Expr[Float] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Float = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Float")
    c.Expr(Apply(Select(Float, "intBitsToFloat"), self :: Nil))(WeakTypeTag.Float)
  }
}
