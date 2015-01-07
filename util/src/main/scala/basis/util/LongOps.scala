//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
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

private[util] class LongMacros(val c: blackbox.Context { type PrefixType <: LongOps }) {
  import c.{ Expr, prefix }
  import c.universe._

  def abs: Expr[Long]                = Expr[Long](q"_root_.java.lang.Math.abs($prefix.__)")
  def countLeadingZeros: Expr[Int]   = Expr[Int](q"_root_.java.lang.Long.numberOfLeadingZeros($prefix.__)")
  def countSetBits: Expr[Int]        = Expr[Int](q"_root_.java.lang.Long.bitCount($prefix.__)")
  def countTrailingZeros: Expr[Int]  = Expr[Int](q"_root_.java.lang.Long.numberOfTrailingZeros($prefix.__)")
  def max(b: Expr[Long]): Expr[Long] = Expr[Long](q"_root_.java.lang.Math.max($prefix.__, $b)")
  def min(b: Expr[Long]): Expr[Long] = Expr[Long](q"_root_.java.lang.Math.min($prefix.__, $b)")
  def signum: Expr[Int]              = Expr[Int](q"_root_.java.lang.Long.signum($prefix.__)")
  def toDoubleBits: Expr[Double]     = Expr[Double](q"_root_.java.lang.Double.longBitsToDouble($prefix.__)")
}
