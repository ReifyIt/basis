/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.util

/** Infix arrow (-> and →) associators. */
final class ArrowOps[+A] {
  def -> [B](right: B): (A, B) = macro ArrowMacros.->[A, B]
  
  def → [B](right: B): (A, B) = macro ArrowMacros.->[A, B]
}

private[util] object ArrowMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def -> [A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(right: c.Expr[B]): c.Expr[(A, B)] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val Apply(_, left :: Nil) = c.prefix.tree
    val PairType = appliedType(mirror.staticClass("scala.Tuple2").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil)
    Expr(New(PairType, left, right.tree))(WeakTypeTag(PairType))
  }
}
