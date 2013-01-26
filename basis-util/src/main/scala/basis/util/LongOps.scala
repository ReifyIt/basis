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
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "abs"),
        unApply(c).tree :: Nil))
  }
  
  def min(c: Context)(b: c.Expr[Long]): c.Expr[Long] = {
    import c.Expr
    import c.universe._
    Expr[Long](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "min"),
        unApply(c).tree :: b.tree :: Nil))
  }
  
  def max(c: Context)(b: c.Expr[Long]): c.Expr[Long] = {
    import c.Expr
    import c.universe._
    Expr[Long](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "max"),
        unApply(c).tree :: b.tree :: Nil))
  }
  
  def signum(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long"), "signum"),
        unApply(c).tree :: Nil))
  }
  
  def countSetBits(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long"), "bitCount"),
        unApply(c).tree :: Nil))
  }
  
  def countLeadingZeros(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long"), "numberOfLeadingZeros"),
        unApply(c).tree :: Nil))
  }
  
  def countTrailingZeros(c: Context): c.Expr[Int] = {
    import c.Expr
    import c.universe._
    Expr[Int](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Long"), "numberOfTrailingZeros"),
        unApply(c).tree :: Nil))
  }
  
  def toDoubleBits(c: Context): c.Expr[Double] = {
    import c.Expr
    import c.universe._
    Expr[Double](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Double"), "longBitsToDouble"),
        unApply(c).tree :: Nil))
  }
}
