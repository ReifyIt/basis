/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

private[basis] object LongMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def MinValue(c: Context): c.Expr[Long] = c.literal(scala.Long.MinValue)
  
  def MaxValue(c: Context): c.Expr[Long] = c.literal(scala.Long.MaxValue)
  
  def abs(c: Context): c.Expr[Long] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "abs"), self :: Nil))(WeakTypeTag.Long)
  }
  
  def min(c: Context)(that: c.Expr[Long]): c.Expr[Long] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "min"), self :: that.tree :: Nil))(WeakTypeTag.Long)
  }
  
  def max(c: Context)(that: c.Expr[Long]): c.Expr[Long] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    c.Expr(Apply(Select(Math, "max"), self :: that.tree :: Nil))(WeakTypeTag.Long)
  }
  
  def signum(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Long = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long")
    c.Expr(Apply(Select(Long, "signum"), self :: Nil))(WeakTypeTag.Int)
  }
  
  def countSetBits(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Long = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long")
    c.Expr(Apply(Select(Long, "bitCount"), self :: Nil))(WeakTypeTag.Int)
  }
  
  def countLeadingZeros(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Long = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long")
    c.Expr(Apply(Select(Long, "numberOfLeadingZeros"), self :: Nil))(WeakTypeTag.Int)
  }
  
  def countTrailingZeros(c: Context): c.Expr[Int] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Long = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long")
    c.Expr(Apply(Select(Long, "numberOfTrailingZeros"), self :: Nil))(WeakTypeTag.Int)
  }
  
  def toDoubleBits(c: Context): c.Expr[Double] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    val Double = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double")
    c.Expr(Apply(Select(Double, "longBitsToDouble"), self :: Nil))(WeakTypeTag.Double)
  }
}
