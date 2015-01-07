//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

/** General purpose utilities. */
package object util {
  /** The default breakable control-flow context. */
  val begin: Begin = new Begin

  implicit def ArrowToOps[A](left: A): ArrowOps[A] = macro UtilMacros.ArrowToOps[A]
  implicit def IntToOps(a: Int): IntOps            = macro UtilMacros.IntToOps
  implicit def LongToOps(a: Long): LongOps         = macro UtilMacros.LongToOps
  implicit def FloatToOps(x: Float): FloatOps      = macro UtilMacros.FloatToOps
  implicit def DoubleToOps(x: Double): DoubleOps   = macro UtilMacros.DoubleToOps
}
