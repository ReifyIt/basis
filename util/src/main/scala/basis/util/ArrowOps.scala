//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

final class ArrowOps[+A](val __ : A) extends AnyVal {
  def -> [B](right: B): (A, B) = macro ArrowMacros.->[A, B]
  def →  [B](right: B): (A, B) = macro ArrowMacros.->[A, B]
}

private[util] class ArrowMacros(val c: blackbox.Context { type PrefixType <: ArrowOps[_] }) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  def -> [A, B](right: Expr[B])(implicit A: WeakTypeTag[A], B: WeakTypeTag[B]): Expr[(A, B)] = {
    implicit val Tuple2ABTag = WeakTypeTag[(A, B)](appliedType(definitions.TupleClass(2), A.tpe :: B.tpe :: Nil))
    Expr[(A, B)](q"($prefix.__, $right)")
  }
}
