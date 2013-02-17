/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.util

/** Infix arrow (-> and →) associators.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
final class ArrowOps[+A](left: A) {
  def -> [B](right: B): (A, B) = macro ArrowMacros.->[A, B]
  
  def → [B](right: B): (A, B) = macro ArrowMacros.->[A, B]
}

private[util] object ArrowMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.{Expr, prefix, typeCheck, weakTypeOf}
    import c.universe._
    val Apply(_, left :: Nil) = prefix.tree
    Expr[A](typeCheck(left, weakTypeOf[A]))
  }
  
  def -> [A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(right: c.Expr[B]): c.Expr[(A, B)] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val Tuple2ABTag =
      WeakTypeTag[(A, B)](
        appliedType(
          mirror.staticClass("scala.Tuple2").toType,
          weakTypeOf[A] :: weakTypeOf[B] :: Nil))
    Expr[(A, B)](
      Apply(
        Select(New(TypeTree(weakTypeOf[(A, B)])), nme.CONSTRUCTOR),
        unApply[A](c).tree :: right.tree :: Nil))
  }
}
