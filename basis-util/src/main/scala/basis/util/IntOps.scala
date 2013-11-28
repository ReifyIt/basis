//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

/** Extended `Int` operations.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
final class IntOps(val __ : Int) {
  def abs: Int = macro IntOps.abs

  def min(b: Int): Int = macro IntOps.min

  def max(b: Int): Int = macro IntOps.max

  def signum: Int = macro IntOps.signum

  def countSetBits: Int = macro IntOps.countSetBits

  def countLeadingZeros: Int = macro IntOps.countLeadingZeros

  def countTrailingZeros: Int = macro IntOps.countTrailingZeros

  def toFloatBits: Float = macro IntOps.toFloatBits
}

private[util] object IntOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply(c: Context): c.Expr[Int] = {
    import c.{ Expr, prefix, typeCheck, weakTypeOf }
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    Expr[Int](typeCheck(a, weakTypeOf[Int]))
  }

  def IntToOps(c: Context)(a: c.Expr[Int]): c.Expr[IntOps] = {
    import c.{ Expr, mirror, WeakTypeTag }
    import c.universe._
    implicit val IntOpsTag =
      WeakTypeTag[IntOps](mirror.staticClass("basis.util.IntOps").toType)
    Expr[IntOps](
      Apply(
        Select(New(TypeTree(weakTypeOf[IntOps])), nme.CONSTRUCTOR),
        a.tree :: Nil))
  }

  def abs(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangMath(c), "abs": TermName),
        unApply(c).tree :: Nil))
  }

  def min(c: Context)(b: c.Expr[Int]): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangMath(c), "min": TermName),
        unApply(c).tree :: b.tree :: Nil))
  }

  def max(c: Context)(b: c.Expr[Int]): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangMath(c), "max": TermName),
        unApply(c).tree :: b.tree :: Nil))
  }

  def signum(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangInteger(c), "signum": TermName),
        unApply(c).tree :: Nil))
  }

  def countSetBits(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangInteger(c), "bitCount": TermName),
        unApply(c).tree :: Nil))
  }

  def countLeadingZeros(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangInteger(c), "numberOfLeadingZeros": TermName),
        unApply(c).tree :: Nil))
  }

  def countTrailingZeros(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangInteger(c), "numberOfTrailingZeros": TermName),
        unApply(c).tree :: Nil))
  }

  def toFloatBits(c: Context): c.Expr[Float] = {
    import c.Expr
    import c.universe._
    Expr[Float](
      Apply(
        Select(JavaLangFloat(c), "intBitsToFloat": TermName),
        unApply(c).tree :: Nil))
  }

  private def JavaLangFloat(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Float": TermName)
  }

  private def JavaLangInteger(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Integer": TermName)
  }

  private def JavaLangMath(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Math": TermName)
  }
}
