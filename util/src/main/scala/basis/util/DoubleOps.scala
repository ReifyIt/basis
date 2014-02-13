//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

/** Extended `Double` operations.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
final class DoubleOps(val __ : Double) extends AnyVal {
  def isNaN: Boolean = macro DoubleOps.isNaN

  def isInfinite: Boolean = macro DoubleOps.isInfinite

  def abs: Double = macro DoubleOps.abs

  def min(y: Double): Double = macro DoubleOps.min

  def max(y: Double): Double = macro DoubleOps.max

  def pow(y: Double): Double = macro DoubleOps.pow

  def exp: Double = macro DoubleOps.exp

  def log: Double = macro DoubleOps.log

  def log10: Double = macro DoubleOps.log10

  def sqrt: Double = macro DoubleOps.sqrt

  def cbrt: Double = macro DoubleOps.cbrt

  def sin: Double = macro DoubleOps.sin

  def cos: Double = macro DoubleOps.cos

  def tan: Double = macro DoubleOps.tan

  def asin: Double = macro DoubleOps.asin

  def acos: Double = macro DoubleOps.acos

  def atan: Double = macro DoubleOps.atan

  def atan(y: Double) = macro DoubleOps.atan2

  def sinh: Double = macro DoubleOps.sinh

  def cosh: Double = macro DoubleOps.cosh

  def tanh: Double = macro DoubleOps.tanh

  def toLongBits: Long = macro DoubleOps.toLongBits
}

private[util] object DoubleOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply(c: Context): c.Expr[Double] = {
    import c.{ Expr, prefix, typeCheck, weakTypeOf }
    import c.universe._
    val Apply(_, x :: Nil) = prefix.tree
    Expr[Double](typeCheck(x, weakTypeOf[Double]))
  }

  def DoubleToOps(c: Context)(x: c.Expr[Double]): c.Expr[DoubleOps] = {
    import c.{ Expr, mirror, WeakTypeTag }
    import c.universe._
    implicit val DoubleOpsTag =
      WeakTypeTag[DoubleOps](mirror.staticClass("basis.util.DoubleOps").toType)
    Expr[DoubleOps](
      Apply(
        Select(New(TypeTree(weakTypeOf[DoubleOps])), nme.CONSTRUCTOR),
        x.tree :: Nil))
  }

  def isNaN(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(JavaLangDouble(c), "isNaN": TermName),
        unApply(c).tree :: Nil))
  }

  def isInfinite(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(JavaLangDouble(c), "isInfinite": TermName),
        unApply(c).tree :: Nil))
  }

  def abs(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "abs": TermName),
        unApply(c).tree :: Nil))
  }

  def min(c: Context)(y: c.Expr[Double]): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "min": TermName),
        unApply(c).tree :: y.tree :: Nil))
  }

  def max(c: Context)(y: c.Expr[Double]): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "max": TermName),
        unApply(c).tree :: y.tree :: Nil))
  }

  def pow(c: Context)(y: c.Expr[Double]): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "pow": TermName),
        unApply(c).tree :: y.tree :: Nil))
  }

  def exp(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "exp": TermName),
        unApply(c).tree :: Nil))
  }

  def log(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "log": TermName),
        unApply(c).tree :: Nil))
  }

  def log10(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "log10": TermName),
        unApply(c).tree :: Nil))
  }

  def sqrt(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "sqrt": TermName),
        unApply(c).tree :: Nil))
  }

  def cbrt(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "cbrt": TermName),
        unApply(c).tree :: Nil))
  }

  def sin(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "sin": TermName),
        unApply(c).tree :: Nil))
  }

  def cos(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "cos": TermName),
        unApply(c).tree :: Nil))
  }

  def tan(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "tan": TermName),
        unApply(c).tree :: Nil))
  }

  def asin(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "asin": TermName),
        unApply(c).tree :: Nil))
  }

  def acos(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "acos": TermName),
        unApply(c).tree :: Nil))
  }

  def atan(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "atan": TermName),
        unApply(c).tree :: Nil))
  }

  def atan2(c: Context)(y: c.Expr[Double]): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "atan2": TermName),
        unApply(c).tree :: y.tree :: Nil))
  }

  def sinh(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "sinh": TermName),
        unApply(c).tree :: Nil))
  }

  def cosh(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "cosh": TermName),
        unApply(c).tree :: Nil))
  }

  def tanh(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangMath(c), "tanh": TermName),
        unApply(c).tree :: Nil))
  }

  def toLongBits(c: Context): c.Expr[Long] = {
    import c.Expr
    import c.universe._
    Expr[Long](
      Apply(
        Select(JavaLangDouble(c), "doubleToLongBits": TermName),
        unApply(c).tree :: Nil))
  }

  private def JavaLangDouble(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Double": TermName)
  }

  private def JavaLangMath(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Math": TermName)
  }
}
