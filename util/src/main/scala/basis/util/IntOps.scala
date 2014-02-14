//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
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

private[util] object IntMacros {
  def IntToOps(c: Context)(a: c.Expr[Int]): c.Expr[IntOps] = {
    import c.universe._
    c.Expr[IntOps](q"new IntOps($a)")
  }

  def abs(c: ContextWithPre[IntOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Math.abs(${c.prefix}.__)")
  }

  def countLeadingZeros(c: ContextWithPre[IntOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Integer.numberOfLeadingZeros(${c.prefix}.__)")
  }

  def countSetBits(c: ContextWithPre[IntOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Integer.bitCount(${c.prefix}.__)")
  }

  def countTrailingZeros(c: ContextWithPre[IntOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Integer.numberOfTrailingZeros(${c.prefix}.__)")
  }

  def max(c: ContextWithPre[IntOps])(b: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Math.max(${c.prefix}.__, $b)")
  }

  def min(c: ContextWithPre[IntOps])(b: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Math.min(${c.prefix}.__, $b)")
  }

  def signum(c: ContextWithPre[IntOps]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"java.lang.Integer.signum(${c.prefix}.__)")
  }

  def toFloatBits(c: ContextWithPre[IntOps]): c.Expr[Float] = {
    import c.universe._
    c.Expr[Float](q"java.lang.Float.intBitsToFloat(${c.prefix}.__)")
  }
}
