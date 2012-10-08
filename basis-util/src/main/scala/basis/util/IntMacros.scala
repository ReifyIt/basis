/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis
package util

private[basis] object IntMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.api.Universe
  import scala.reflect.macros.Context
  
  def abs(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    c.Expr {
      Apply(Select(JavaLangMath(c.universe), newTermName("abs")), self :: Nil)
    } (WeakTypeTag.Int)
  }
  
  def max(c: Context)(that: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    c.Expr {
      Apply(Select(JavaLangMath(c.universe), newTermName("max")), self :: that.tree :: Nil)
    } (WeakTypeTag.Int)
  }
  
  def min(c: Context)(that: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    c.Expr {
      Apply(Select(JavaLangMath(c.universe), newTermName("min")), self :: that.tree :: Nil)
    } (WeakTypeTag.Int)
  }
  
  def bitCount(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    c.Expr {
      Apply(Select(JavaLangInteger(c.universe), newTermName("bitCount")), self :: Nil)
    } (WeakTypeTag.Int)
  }
  
  def to(c: Context)(end: c.Expr[Int]): c.Expr[Range] = {
    import c.universe._
    val Apply(_, start :: Nil) = c.prefix.tree
    c.Expr {
      Apply(Select(New(BasisUtilRange(c.universe)), nme.CONSTRUCTOR),
            start :: end.tree :: Literal(Constant(1)) :: Literal(Constant(true)) :: Nil)
    } (WeakTypeTag.Nothing)
  }
  
  def until(c: Context)(end: c.Expr[Int]): c.Expr[Range] = {
    import c.universe._
    val Apply(_, start :: Nil) = c.prefix.tree
    c.Expr {
      Apply(Select(New(BasisUtilRange(c.universe)), nme.CONSTRUCTOR),
            start :: end.tree :: Literal(Constant(1)) :: Literal(Constant(false)) :: Nil)
    } (WeakTypeTag.Nothing)
  }
  
  private def BasisUtilRange(u: Universe): u.Tree = {
    import u._
    Select(Select(Select(Ident(nme.ROOTPKG), newTermName("basis")), newTermName("util")), newTypeName("Range"))
  }
  
  private def JavaLangMath(u: Universe): u.Tree = {
    import u._
    Select(Select(Select(Ident(nme.ROOTPKG), newTermName("java")), newTermName("lang")), newTermName("Math"))
  }
  
  private def JavaLangInteger(u: Universe): u.Tree = {
    import u._
    Select(Select(Select(Ident(nme.ROOTPKG), newTermName("java")), newTermName("lang")), newTermName("Integer"))
  }
}
