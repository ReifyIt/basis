//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

final class ArrowOps[+A](val __ : A) extends AnyVal {
  def -> [B](right: B): (A, B) = macro ArrowMacros.->[A, B]
  def → [B](right: B): (A, B)  = macro ArrowMacros.->[A, B]
}

private[util] object ArrowMacros {
  def ArrowToOps[A: c.WeakTypeTag](c: Context)(left: c.Expr[A]): c.Expr[ArrowOps[A]] = {
    import c.universe._
    c.Expr[ArrowOps[A]](q"new ArrowOps($left)")
  }

  def -> [A: c.WeakTypeTag, B: c.WeakTypeTag](c: ContextWithPre[ArrowOps[A]])(right: c.Expr[B]): c.Expr[(A, B)] = {
    import c.universe._
    c.Expr[(A, B)](q"(${c.prefix}.__, $right)")
  }
}
