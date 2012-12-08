/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

private[containers] object ListMacros {
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
    import c.{mirror, WeakTypeTag}
    import c.universe._
    val ListTpc = mirror.staticClass("basis.containers.immutable.List").toType
    WeakTypeTag(appliedType(ListTpc, weakTypeOf[A] :: Nil))
  }
}
