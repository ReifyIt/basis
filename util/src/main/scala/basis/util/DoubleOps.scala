//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

final class DoubleOps(val __ : Double) extends AnyVal {
  def abs: Double             = macro DoubleMacros.abs
  def acos: Double            = macro DoubleMacros.acos
  def asin: Double            = macro DoubleMacros.asin
  def atan: Double            = macro DoubleMacros.atan
  def atan(y: Double): Double = macro DoubleMacros.atan2
  def cbrt: Double            = macro DoubleMacros.cbrt
  def cos: Double             = macro DoubleMacros.cos
  def cosh: Double            = macro DoubleMacros.cosh
  def exp: Double             = macro DoubleMacros.exp
  def isInfinite: Boolean     = macro DoubleMacros.isInfinite
  def isNaN: Boolean          = macro DoubleMacros.isNaN
  def log10: Double           = macro DoubleMacros.log10
  def log: Double             = macro DoubleMacros.log
  def max(y: Double): Double  = macro DoubleMacros.max
  def min(y: Double): Double  = macro DoubleMacros.min
  def pow(y: Double): Double  = macro DoubleMacros.pow
  def sin: Double             = macro DoubleMacros.sin
  def sinh: Double            = macro DoubleMacros.sinh
  def sqrt: Double            = macro DoubleMacros.sqrt
  def tan: Double             = macro DoubleMacros.tan
  def tanh: Double            = macro DoubleMacros.tanh
  def toLongBits: Long        = macro DoubleMacros.toLongBits
  def toRawLongBits: Long     = macro DoubleMacros.toRawLongBits
}

private[util] class DoubleMacros(val c: blackbox.Context { type PrefixType <: DoubleOps }) {
  import c.{ Expr, prefix }
  import c.universe._

  def abs: Expr[Double]                    = Expr[Double](q"_root_.java.lang.Math.abs($prefix.__)")
  def acos: Expr[Double]                   = Expr[Double](q"_root_.java.lang.Math.acos($prefix.__)")
  def asin: Expr[Double]                   = Expr[Double](q"_root_.java.lang.Math.asin($prefix.__)")
  def atan: Expr[Double]                   = Expr[Double](q"_root_.java.lang.Math.atan($prefix.__)")
  def atan2(y: Expr[Double]): Expr[Double] = Expr[Double](q"_root_.java.lang.Math.atan2($prefix.__, $y)")
  def cbrt: Expr[Double]                   = Expr[Double](q"_root_.java.lang.Math.cbrt($prefix.__)")
  def cos: Expr[Double]                    = Expr[Double](q"_root_.java.lang.Math.cose($prefix.__)")
  def cosh: Expr[Double]                   = Expr[Double](q"_root_.java.lang.Math.cosh($prefix.__)")
  def exp: Expr[Double]                    = Expr[Double](q"_root_.java.lang.Math.exp($prefix.__)")
  def isInfinite: Expr[Boolean]            = Expr[Boolean](q"_root_.java.lang.Double.isInfinite($prefix.__)")
  def isNaN: Expr[Boolean]                 = Expr[Boolean](q"_root_.java.lang.Double.isNaN($prefix.__)")
  def log10: Expr[Double]                  = Expr[Double](q"_root_.java.lang.Math.log10($prefix.__)")
  def log: Expr[Double]                    = Expr[Double](q"_root_.java.lang.Math.log($prefix.__)")
  def max(y: Expr[Double]): Expr[Double]   = Expr[Double](q"_root_.java.lang.Math.max($prefix.__, $y)")
  def min(y: Expr[Double]): Expr[Double]   = Expr[Double](q"_root_.java.lang.Math.min($prefix.__, $y)")
  def pow(y: Expr[Double]): Expr[Double]   = Expr[Double](q"_root_.java.lang.Math.pow($prefix.__, $y)")
  def sin: Expr[Double]                    = Expr[Double](q"_root_.java.lang.Math.sin($prefix.__)")
  def sinh: Expr[Double]                   = Expr[Double](q"_root_.java.lang.Math.sinh($prefix.__)")
  def sqrt: Expr[Double]                   = Expr[Double](q"_root_.java.lang.Math.sqrt($prefix.__)")
  def tan: Expr[Double]                    = Expr[Double](q"_root_.java.lang.Math.tan($prefix.__)")
  def tanh: Expr[Double]                   = Expr[Double](q"_root_.java.lang.Math.tanh($prefix.__)")
  def toLongBits: Expr[Long]               = Expr[Long](q"_root_.java.lang.Double.doubleToLongBits($prefix.__)")
  def toRawLongBits: Expr[Long]            = Expr[Long](q"_root_.java.lang.Double.doubleToRawLongBits($prefix.__)")
}
