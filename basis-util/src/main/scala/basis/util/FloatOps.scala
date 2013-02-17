/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.util

/** Extended `Float` operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
final class FloatOps(x: Float) {
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
  
  private def unApply(c: Context): c.Expr[Float] = {
    import c.{Expr, prefix, typeCheck, weakTypeOf}
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    Expr[Float](typeCheck(x, weakTypeOf[Float]))
  }
  
  def isNaN(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(JavaLangFloat(c), "isNaN": TermName),
        unApply(c).tree :: Nil))
  }
  
  def isInfinite(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(JavaLangFloat(c), "isInfinite": TermName),
        unApply(c).tree :: Nil))
  }
  
  def abs(c: Context): c.Expr[Float] = {
    import c.Expr
    import c.universe._
    Expr[Float](
      Apply(
        Select(JavaLangMath(c), "abs": TermName),
        unApply(c).tree :: Nil))
  }
  
  def min(c: Context)(y: c.Expr[Float]): c.Expr[Float] = {
    import c.Expr
    import c.universe._
    Expr[Float](
      Apply(
        Select(JavaLangMath(c), "min": TermName),
        unApply(c).tree :: y.tree :: Nil))
  }
  
  def max(c: Context)(y: c.Expr[Float]): c.Expr[Float] = {
    import c.Expr
    import c.universe._
    Expr[Float](
      Apply(
        Select(JavaLangMath(c), "max": TermName),
        unApply(c).tree :: y.tree :: Nil))
  }
  
  def toIntBits(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangFloat(c), "floatToIntBits": TermName),
        unApply(c).tree :: Nil))
  }
  
  private def JavaLangFloat(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Float": TermName)
  }
  
  private def JavaLangMath(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Math": TermName)
  }
}
