/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

/** Supplemental operations on `Int` values. */
final class IntOps {
  def abs: Int = macro IntMacros.abs
  
  def min(b: Int): Int = macro IntMacros.min
  
  def max(b: Int): Int = macro IntMacros.max
  
  def signum: Int = macro IntMacros.signum
  
  def countSetBits: Int = macro IntMacros.countSetBits
  
  def countLeadingZeros: Int = macro IntMacros.countLeadingZeros
  
  def countTrailingZeros: Int = macro IntMacros.countTrailingZeros
  
  def toFloatBits: Float = macro IntMacros.toFloatBits
}

private[util] object IntMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def abs(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "abs"), a :: Nil))(TypeTag.Int)
  }
  
  def min(c: Context)(b: c.Expr[Int]): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "min"), a :: b.tree :: Nil))(TypeTag.Int)
  }
  
  def max(c: Context)(b: c.Expr[Int]): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "max"), a :: b.tree :: Nil))(TypeTag.Int)
  }
  
  def signum(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Integer = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer")
    Expr(Apply(Select(Integer, "signum"), a :: Nil))(TypeTag.Int)
  }
  
  def countSetBits(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Integer = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer")
    Expr(Apply(Select(Integer, "bitCount"), a :: Nil))(TypeTag.Int)
  }
  
  def countLeadingZeros(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Integer = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer")
    Expr(Apply(Select(Integer, "numberOfLeadingZeros"), a :: Nil))(TypeTag.Int)
  }
  
  def countTrailingZeros(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Integer = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer")
    Expr(Apply(Select(Integer, "numberOfTrailingZeros"), a :: Nil))(TypeTag.Int)
  }
  
  def toFloatBits(c: Context): c.Expr[Float] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    val Float = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Float")
    Expr(Apply(Select(Float, "intBitsToFloat"), a :: Nil))(TypeTag.Float)
  }
}
