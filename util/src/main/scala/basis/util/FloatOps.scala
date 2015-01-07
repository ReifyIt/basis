//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

final class FloatOps(val __ : Float) extends AnyVal {
  def abs: Float           = macro FloatMacros.abs
  def isInfinite: Boolean  = macro FloatMacros.isInfinite
  def isNaN: Boolean       = macro FloatMacros.isNaN
  def max(y: Float): Float = macro FloatMacros.max
  def min(y: Float): Float = macro FloatMacros.min
  def toIntBits: Int       = macro FloatMacros.toIntBits
  def toRawIntBits: Int    = macro FloatMacros.toRawIntBits
}

private[util] class FloatMacros(val c: blackbox.Context { type PrefixType <: FloatOps }) {
  import c.{ Expr, prefix }
  import c.universe._

  def abs: Expr[Float]                 = Expr[Float](q"_root_.java.lang.Math.abs($prefix.__)")
  def isInfinite: Expr[Boolean]        = Expr[Boolean](q"_root_.java.lang.Float.isInfinite($prefix.__)")
  def isNaN: Expr[Boolean]             = Expr[Boolean](q"_root_.java.lang.Float.isNaN($prefix.__)")
  def max(y: Expr[Float]): Expr[Float] = Expr[Float](q"_root_.java.lang.Math.max($prefix.__, $y)")
  def min(y: Expr[Float]): Expr[Float] = Expr[Float](q"_root_.java.lang.Math.min($prefix.__, $y)")
  def toIntBits: Expr[Int]             = Expr[Int](q"_root_.java.lang.Float.floatToIntBits($prefix.__)")
  def toRawIntBits: Expr[Int]          = Expr[Int](q"_root_.java.lang.Float.floatToRawIntBits($prefix.__)")
}
