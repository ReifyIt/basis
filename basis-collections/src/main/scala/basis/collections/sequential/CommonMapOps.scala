/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Common map operations.
  * 
  * @groupprio  Traversing    -3
  * @groupprio  Reducing      -2
  * @groupprio  Querying      -1
  */
abstract class CommonMapOps[+Self, +A, +T] private[sequential] {
  def eagerly: EagerMapOps[Self, A, T] =
    macro CommonMapOps.eagerly[Self, A, T]
  
  def lazily: LazyMapOps[A, T] =
    macro CommonMapOps.lazily[A, T]
}

private[sequential] object CommonMapOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def deconstruct(c: Context): c.Tree = {
    import c.universe._
    val Apply(_, map :: Nil) = c.prefix.tree
    map
  }
  
  def eagerly[Self : c.WeakTypeTag, A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[EagerMapOps[Self, A, T]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "EagerMapOps"),
        deconstruct(c) :: Nil)
    } (EagerMapOpsTag[Self, A, T](c))
  }
  
  def lazily[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[LazyMapOps[A, T]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "LazyMapOps"),
        deconstruct(c) :: Nil)
    } (LazyMapOpsTag[A, T](c))
  }
  
  private def EagerMapOpsTag[Self : c.WeakTypeTag, A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[EagerMapOps[Self, A, T]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.EagerMapOps").toType,
        weakTypeOf[Self] :: weakTypeOf[A] :: weakTypeOf[T] :: Nil))
  }
  
  private def LazyMapOpsTag[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[LazyMapOps[A, T]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.LazyMapOps").toType,
        weakTypeOf[A] :: weakTypeOf[T] :: Nil))
  }
}
