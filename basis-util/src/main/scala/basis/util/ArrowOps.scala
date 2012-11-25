/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

/** Infix arrow (-> and →) associators. */
trait ArrowOps[A] extends Any {
  def -> [B](right: B): (A, B) = macro ArrowMacros.->[A, B]
  
  def → [B](right: B): (A, B) = macro ArrowMacros.->[A, B]
}

private[util] object ArrowMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val Apply(_, left :: Nil) = c.prefix.tree
    c.Expr(c.typeCheck(left, c.weakTypeOf[A]))(c.weakTypeTag[A])
  }
  
  def -> [A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(right: c.Expr[B]): c.Expr[(A, B)] =
    c.universe.reify((unApply[A](c).splice, right.splice))
}
