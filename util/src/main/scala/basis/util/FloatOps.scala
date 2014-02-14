//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
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
}

private[util] object FloatMacros {
  def FloatToOps(c: Context)(x: c.Expr[Float]): c.Expr[FloatOps] = {
    import c.universe._
    c.Expr[FloatOps](q"new FloatOps($x)")
  }

  def abs(c: ContextWithPre[FloatOps]): c.Expr[Float] = {
    import c.universe._
    c.Expr[Float](q"java.lang.Math.abs(${c.prefix}.__)")
  }

  def isInfinite(c: ContextWithPre[FloatOps]): c.Expr[Boolean] = {
    import c.universe._
    c.Expr[Boolean](q"java.lang.Float.isInfinite(${c.prefix}.__)")
  }

  def isNaN(c: ContextWithPre[FloatOps]): c.Expr[Boolean] = {
    import c.universe._
    c.Expr[Boolean](q"java.lang.Float.isNaN(${c.prefix}.__)")
  }

  def max(c: ContextWithPre[FloatOps])(y: c.Expr[Float]): c.Expr[Float] = {
    import c.universe._
    c.Expr[Float](q"java.lang.Math.max(${c.prefix}.__, $y)")
  }

  def min(c: ContextWithPre[FloatOps])(y: c.Expr[Float]): c.Expr[Float] = {
    import c.universe._
    c.Expr[Float](q"java.lang.Math.min(${c.prefix}.__, $y)")
  }

  def toIntBits(c: ContextWithPre[FloatOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Float.floatToIntBits(${c.prefix}.__)")
  }
}
