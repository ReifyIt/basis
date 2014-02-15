//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

final class DoubleOps(val __ : Double) extends AnyVal {
  def abs: Double            = macro DoubleMacros.abs
  def acos: Double           = macro DoubleMacros.acos
  def asin: Double           = macro DoubleMacros.asin
  def atan: Double           = macro DoubleMacros.atan
  def atan(y: Double)        = macro DoubleMacros.atan2
  def cbrt: Double           = macro DoubleMacros.cbrt
  def cos: Double            = macro DoubleMacros.cos
  def cosh: Double           = macro DoubleMacros.cosh
  def exp: Double            = macro DoubleMacros.exp
  def isInfinite: Boolean    = macro DoubleMacros.isInfinite
  def isNaN: Boolean         = macro DoubleMacros.isNaN
  def log10: Double          = macro DoubleMacros.log10
  def log: Double            = macro DoubleMacros.log
  def max(y: Double): Double = macro DoubleMacros.max
  def min(y: Double): Double = macro DoubleMacros.min
  def pow(y: Double): Double = macro DoubleMacros.pow
  def sin: Double            = macro DoubleMacros.sin
  def sinh: Double           = macro DoubleMacros.sinh
  def sqrt: Double           = macro DoubleMacros.sqrt
  def tan: Double            = macro DoubleMacros.tan
  def tanh: Double           = macro DoubleMacros.tanh
  def toLongBits: Long       = macro DoubleMacros.toLongBits
  def toRawLongBits: Long    = macro DoubleMacros.toRawLongBits
}

private[util] object DoubleMacros {
  def DoubleToOps(c: Context)(x: c.Expr[Double]): c.Expr[DoubleOps] = {
    import c.universe._
    c.Expr[DoubleOps](q"new DoubleOps($x)")
  }

  def abs(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.abs(${c.prefix}.__)")
  }

  def acos(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.acos(${c.prefix}.__)")
  }

  def asin(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.asin(${c.prefix}.__)")
  }

  def atan(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.atan(${c.prefix}.__)")
  }

  def atan2(c: ContextWithPre[DoubleOps])(y: c.Expr[Double]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.atan2(${c.prefix}.__, $y)")
  }

  def cbrt(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.cbrt(${c.prefix}.__)")
  }

  def cos(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.cose(${c.prefix}.__)")
  }

  def cosh(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.cosh(${c.prefix}.__)")
  }

  def exp(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.exp(${c.prefix}.__)")
  }

  def isInfinite(c: ContextWithPre[DoubleOps]): c.Expr[Boolean] = {
    import c.universe._
    c.Expr[Boolean](q"java.lang.Double.isInfinite(${c.prefix}.__)")
  }

  def isNaN(c: ContextWithPre[DoubleOps]): c.Expr[Boolean] = {
    import c.universe._
    c.Expr[Boolean](q"java.lang.Double.isNaN(${c.prefix}.__)")
  }

  def log10(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.log10(${c.prefix}.__)")
  }

  def log(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.log(${c.prefix}.__)")
  }

  def max(c: ContextWithPre[DoubleOps])(y: c.Expr[Double]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.max(${c.prefix}.__, $y)")
  }

  def min(c: ContextWithPre[DoubleOps])(y: c.Expr[Double]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.min(${c.prefix}.__, $y)")
  }

  def pow(c: ContextWithPre[DoubleOps])(y: c.Expr[Double]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.pow(${c.prefix}.__, $y)")
  }

  def sin(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.sin(${c.prefix}.__)")
  }

  def sinh(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.sinh(${c.prefix}.__)")
  }

  def sqrt(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.sqrt(${c.prefix}.__)")
  }

  def tan(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.tan(${c.prefix}.__)")
  }

  def tanh(c: ContextWithPre[DoubleOps]): c.Expr[Double] = {
    import c.universe._
    c.Expr[Double](q"java.lang.Math.tanh(${c.prefix}.__)")
  }

  def toLongBits(c: ContextWithPre[DoubleOps]): c.Expr[Long] = {
    import c.universe._
    c.Expr[Long](q"java.lang.Double.doubleToLongBits(${c.prefix}.__)")
  }

  def toRawLongBits(c: ContextWithPre[DoubleOps]): c.Expr[Long] = {
    import c.universe._
    c.Expr[Long](q"java.lang.Double.doubleToRawLongBits(${c.prefix}.__)")
  }
}
