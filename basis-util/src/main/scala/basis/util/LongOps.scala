/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.util

/** Extended `Long` operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
final class LongOps(a: Long) {
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
  
  private def unApply(c: Context): c.Expr[Long] = {
    import c.{Expr, prefix, typeCheck, weakTypeOf}
    import c.universe._
    val Apply(_, a :: Nil) = prefix.tree
    Expr[Long](typeCheck(a, weakTypeOf[Long]))
  }
  
  def abs(c: Context): c.Expr[Long] = {
    import c.Expr
    import c.universe._
    Expr[Long](
      Apply(
        Select(JavaLangMath(c), "abs": TermName),
        unApply(c).tree :: Nil))
  }
  
  def min(c: Context)(b: c.Expr[Long]): c.Expr[Long] = {
    import c.Expr
    import c.universe._
    Expr[Long](
      Apply(
        Select(JavaLangMath(c), "min": TermName),
        unApply(c).tree :: b.tree :: Nil))
  }
  
  def max(c: Context)(b: c.Expr[Long]): c.Expr[Long] = {
    import c.Expr
    import c.universe._
    Expr[Long](
      Apply(
        Select(JavaLangMath(c), "max": TermName),
        unApply(c).tree :: b.tree :: Nil))
  }
  
  def signum(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangLong(c), "signum": TermName),
        unApply(c).tree :: Nil))
  }
  
  def countSetBits(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangLong(c), "bitCount": TermName),
        unApply(c).tree :: Nil))
  }
  
  def countLeadingZeros(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangLong(c), "numberOfLeadingZeros": TermName),
        unApply(c).tree :: Nil))
  }
  
  def countTrailingZeros(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(JavaLangLong(c), "numberOfTrailingZeros": TermName),
        unApply(c).tree :: Nil))
  }
  
  def toDoubleBits(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(JavaLangDouble(c), "longBitsToDouble": TermName),
        unApply(c).tree :: Nil))
  }
  
  private def JavaLangDouble(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Double": TermName)
  }
  
  private def JavaLangLong(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Long": TermName)
  }
  
  private def JavaLangMath(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName), "Math": TermName)
  }
}
