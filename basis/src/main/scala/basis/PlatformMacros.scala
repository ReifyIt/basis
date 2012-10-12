/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

private[basis] object PlatformMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def equal[T]
      (c: Context)
      (x: c.Expr[T], y: c.Expr[T])
      (T: c.Expr[Equal[T]])
    : c.Expr[Boolean] = {
    import c.universe._
    c.Expr(Apply(Select(T.tree, "equal"), x.tree :: y.tree :: Nil))(WeakTypeTag.Boolean)
  }
  
  def hash[T]
      (c: Context)
      (x: c.Expr[T])
      (T: c.Expr[Hash[T]])
    : c.Expr[Int] = {
    import c.universe._
    c.Expr(Apply(Select(T.tree, "hash"), x.tree :: Nil))(WeakTypeTag.Int)
  }
  
  def show[T]
      (c: Context)
      (x: c.Expr[T])
      (T: c.Expr[Show[T]], buffer: c.Expr[CharBuffer])
    : c.Expr[buffer.value.State] = {
    import c.universe._
    c.Expr(Apply(Apply(Select(T.tree, "show"), x.tree :: Nil), buffer.tree :: Nil))(WeakTypeTag.Nothing)
  }
}
