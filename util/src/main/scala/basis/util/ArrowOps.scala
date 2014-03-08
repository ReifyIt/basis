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

private[util] object ArrowMacros {
  def -> [A: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context { type PrefixType <: ArrowOps[A] })(right: c.Expr[B]): c.Expr[(A, B)] = {
    import c.{ Expr, prefix, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val Tuple2ABTag = WeakTypeTag[(A, B)](appliedType(definitions.TupleClass(2), weakTypeOf[A] :: weakTypeOf[B] :: Nil))
    Expr[(A, B)](q"($prefix.__, $right)")
  }
}
