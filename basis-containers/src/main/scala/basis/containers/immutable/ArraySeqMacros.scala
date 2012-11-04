/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.memory._

private[containers] object ArraySeqMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[A : c.WeakTypeTag]
      (c: Context)
      (xs: c.Expr[A]*)
      (A: c.Expr[DataType[A]])
    : c.Expr[ArraySeq[A]] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val ArraySeqType = appliedType(mirror.staticClass("basis.containers.immutable.ArraySeq").toType, weakTypeOf[A] :: Nil)
    val buffer =
      Apply(
        TypeApply(
          Select(Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "immutable"), "ArraySeq"), "Builder"),
          TypeTree(weakTypeOf[A]) :: Nil),
        A.tree :: Nil)
    var b = Apply(Select(buffer, "expect"), Literal(Constant(xs.length)) :: Nil)
    val iter = xs.iterator
    while (iter.hasNext) b = Apply(Select(b, "$plus$eq"), iter.next().tree :: Nil)
    Expr(Select(b, "state"))(WeakTypeTag(ArraySeqType))
  }
}
