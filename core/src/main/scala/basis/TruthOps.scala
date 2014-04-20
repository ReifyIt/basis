//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

import scala.reflect.macros._

final class TruthOps(val __ : Truth) extends AnyVal {
  /** Returns `true` if this is the `true` value of the `Truth`, otherwise returns `false`.
    * @group Evaluating */
  def isTrue: Boolean = macro TruthMacros.isTrue

  /** Returns `true` if this is the `false` value of the `Truth`, otherwise returns `false`.
    * @group Evaluating */
  def isFalse: Boolean = macro TruthMacros.isFalse
}

private[basis] class TruthMacros(val c: blackbox.Context { type PrefixType <: TruthOps }) {
  import c.{ Expr, prefix }
  import c.universe._

  def isTrue: Expr[Boolean]  = Expr[Boolean](q"_root_.basis.True eq $prefix.__")
  def isFalse: Expr[Boolean] = Expr[Boolean](q"_root_.basis.False eq $prefix.__")
}
