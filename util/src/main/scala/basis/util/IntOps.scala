//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

final class IntOps(val __ : Int) extends AnyVal {
  def abs: Int                = macro IntMacros.abs
  def countLeadingZeros: Int  = macro IntMacros.countLeadingZeros
  def countSetBits: Int       = macro IntMacros.countSetBits
  def countTrailingZeros: Int = macro IntMacros.countTrailingZeros
  def max(b: Int): Int        = macro IntMacros.max
  def min(b: Int): Int        = macro IntMacros.min
  def signum: Int             = macro IntMacros.signum
  def toFloatBits: Float      = macro IntMacros.toFloatBits
}

private[util] class IntMacros(val c: blackbox.Context { type PrefixType <: IntOps }) {
  import c.{ Expr, prefix }
  import c.universe._

  def abs: Expr[Int]                 = Expr[Int](q"_root_.java.lang.Math.abs($prefix.__)")
  def countLeadingZeros: Expr[Int]   = Expr[Int](q"_root_.java.lang.Integer.numberOfLeadingZeros($prefix.__)")
  def countSetBits: Expr[Int]        = Expr[Int](q"_root_.java.lang.Integer.bitCount($prefix.__)")
  def countTrailingZeros: Expr[Int]  = Expr[Int](q"_root_.java.lang.Integer.numberOfTrailingZeros($prefix.__)")
  def max(b: c.Expr[Int]): Expr[Int] = Expr[Int](q"_root_.java.lang.Math.max($prefix.__, $b)")
  def min(b: c.Expr[Int]): Expr[Int] = Expr[Int](q"_root_.java.lang.Math.min($prefix.__, $b)")
  def signum: Expr[Int]              = Expr[Int](q"_root_.java.lang.Integer.signum($prefix.__)")
  def toFloatBits: Expr[Float]       = Expr[Float](q"_root_.java.lang.Float.intBitsToFloat($prefix.__)")
}
