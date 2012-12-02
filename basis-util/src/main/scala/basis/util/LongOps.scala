/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

/** Supplemental operations on `Long` values. */
final class LongOps {
  def abs: Long = macro LongMacros.abs
  
  def min(b: Long): Long = macro LongMacros.min
  
  def max(b: Long): Long = macro LongMacros.max
  
  def signum: Int = macro LongMacros.signum
  
  def countSetBits: Int = macro LongMacros.countSetBits
  
  def countLeadingZeros: Int = macro LongMacros.countLeadingZeros
  
  def countTrailingZeros: Int = macro LongMacros.countTrailingZeros
  
  def toDoubleBits: Double = macro LongMacros.toDoubleBits
}

private[util] object LongMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def abs(c: Context): c.Expr[Long] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "abs"), a :: Nil))(TypeTag.Long)
  }
  
  def min(c: Context)(b: c.Expr[Long]): c.Expr[Long] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "min"), a :: b.tree :: Nil))(TypeTag.Long)
  }
  
  def max(c: Context)(b: c.Expr[Long]): c.Expr[Long] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "max"), a :: b.tree :: Nil))(TypeTag.Long)
  }
  
  def signum(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Long = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long")
    Expr(Apply(Select(Long, "signum"), a :: Nil))(TypeTag.Int)
  }
  
  def countSetBits(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Long = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long")
    Expr(Apply(Select(Long, "bitCount"), a :: Nil))(TypeTag.Int)
  }
  
  def countLeadingZeros(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Long = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long")
    Expr(Apply(Select(Long, "numberOfLeadingZeros"), a :: Nil))(TypeTag.Int)
  }
  
  def countTrailingZeros(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Long = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long")
    Expr(Apply(Select(Long, "numberOfTrailingZeros"), a :: Nil))(TypeTag.Int)
  }
  
  def toDoubleBits(c: Context): c.Expr[Double] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Double = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double")
    Expr(Apply(Select(Double, "longBitsToDouble"), a :: Nil))(TypeTag.Double)
  }
}
