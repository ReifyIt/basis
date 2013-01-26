/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.util

/** Extended `Int` operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
final class IntOps(a: Int) {
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
  
  private def unApply(c: Context): c.Expr[Int] = {
    import c.{Expr, prefix, typeCheck, weakTypeOf}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    Expr[Int](typeCheck(a, weakTypeOf[Int]))
  }
  
  def abs(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "abs"),
        unApply(c).tree :: Nil))
  }
  
  def min(c: Context)(b: c.Expr[Int]): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "min"),
        unApply(c).tree :: b.tree :: Nil))
  }
  
  def max(c: Context)(b: c.Expr[Int]): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "max"),
        unApply(c).tree :: b.tree :: Nil))
  }
  
  def signum(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer"), "signum"),
        unApply(c).tree :: Nil))
  }
  
  def countSetBits(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer"), "bitCount"),
        unApply(c).tree :: Nil))
  }
  
  def countLeadingZeros(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer"), "numberOfLeadingZeros"),
        unApply(c).tree :: Nil))
  }
  
  def countTrailingZeros(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Integer"), "numberOfTrailingZeros"),
        unApply(c).tree :: Nil))
  }
  
  def toFloatBits(c: Context): c.Expr[Float] = {
    import c.Expr
    import c.universe._
    Expr[Float](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Float"), "intBitsToFloat"),
        unApply(c).tree :: Nil))
  }
}
