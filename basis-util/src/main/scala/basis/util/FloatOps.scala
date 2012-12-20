/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.util

/** Supplemental operations on `Float` values. */
final class FloatOps extends {
  def isNaN: Boolean = macro FloatMacros.isNaN
  
  def isInfinite: Boolean = macro FloatMacros.isInfinite
  
  def abs: Float = macro FloatMacros.abs
  
  def min(y: Float): Float = macro FloatMacros.min
  
  def max(y: Float): Float = macro FloatMacros.max
  
  def toIntBits: Int = macro FloatMacros.toIntBits
}

private[util] object FloatMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def isNaN(c: Context): c.Expr[Boolean] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Float = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Float")
    Expr(Apply(Select(Float, "isNaN"), x :: Nil))(TypeTag.Boolean)
  }
  
  def isInfinite(c: Context): c.Expr[Boolean] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Float = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Float")
    Expr(Apply(Select(Float, "isInfinite"), x :: Nil))(TypeTag.Boolean)
  }
  
  def abs(c: Context): c.Expr[Float] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "abs"), x :: Nil))(TypeTag.Float)
  }
  
  def min(c: Context)(y: c.Expr[Float]): c.Expr[Float] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "min"), x :: y.tree :: Nil))(TypeTag.Float)
  }
  
  def max(c: Context)(y: c.Expr[Float]): c.Expr[Float] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Math = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math")
    Expr(Apply(Select(Math, "max"), x :: y.tree :: Nil))(TypeTag.Float)
  }
  
  def toIntBits(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    val Float = Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Float")
    Expr(Apply(Select(Float, "floatToIntBits"), x :: Nil))(TypeTag.Int)
  }
}
