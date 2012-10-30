/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._

private[immutable] object ListMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[A](c: Context)(xs: c.Expr[A]*): c.Expr[List[A]] = {
    import c.universe._
    var list: c.Tree = Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "immutable"), "Nil")
    val iter = xs.reverseIterator
    while (iter.hasNext) list = Apply(Select(list, "$colon$colon"), iter.next().tree :: Nil)
    c.Expr(list)(ListTag[A](c))
  }
  
  private def ListTag[A : c.WeakTypeTag](c: Context): c.WeakTypeTag[List[A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.containers.immutable.List").toType,
        weakTypeOf[A] :: Nil))
  }
}
