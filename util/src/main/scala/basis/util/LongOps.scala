//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

final class LongOps(val __ : Long) extends AnyVal {
  def abs: Long               = macro LongMacros.abs
  def countLeadingZeros: Int  = macro LongMacros.countLeadingZeros
  def countSetBits: Int       = macro LongMacros.countSetBits
  def countTrailingZeros: Int = macro LongMacros.countTrailingZeros
  def max(b: Long): Long      = macro LongMacros.max
  def min(b: Long): Long      = macro LongMacros.min
  def signum: Int             = macro LongMacros.signum
  def toDoubleBits: Double    = macro LongMacros.toDoubleBits
}

private[util] object LongMacros {
  def LongToOps(c: Context)(a: c.Expr[Long]): c.Expr[LongOps] = {
    import c.universe._
    c.Expr[LongOps](q"new LongOps($a)")
  }

  def abs(c: ContextWithPre[LongOps]): c.Expr[Long] = {
    import c.universe._
    c.Expr[Long](q"java.lang.Math.abs(${c.prefix}.__)")
  }

  def countLeadingZeros(c: ContextWithPre[LongOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Long.numberOfLeadingZeros(${c.prefix}.__)")
  }

  def countSetBits(c: ContextWithPre[LongOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Long.bitCount(${c.prefix}.__)")
  }

  def countTrailingZeros(c: ContextWithPre[LongOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Long.numberOfTrailingZeros(${c.prefix}.__)")
  }

  def max(c: ContextWithPre[LongOps])(b: c.Expr[Long]): c.Expr[Long] = {
    import c.universe._
    c.Expr[Long](q"java.lang.Math.max(${c.prefix}.__, $b)")
  }

  def min(c: ContextWithPre[LongOps])(b: c.Expr[Long]): c.Expr[Long] = {
    import c.universe._
    c.Expr[Long](q"java.lang.Math.min(${c.prefix}.__, $b)")
  }

  def signum(c: ContextWithPre[LongOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Long.signum(${c.prefix}.__)")
  }

  def toDoubleBits(c: Context): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Double.longBitsToDouble(${c.prefix}.__)")
  }
}
