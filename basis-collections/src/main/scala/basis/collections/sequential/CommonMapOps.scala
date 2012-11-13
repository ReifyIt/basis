/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.collections.general._

/** Common map operations.
  * 
  * @groupprio  Traversing    -3
  * @groupprio  Reducing      -2
  * @groupprio  Querying      -1
  */
abstract class CommonMapOps[A, T, From] private[sequential] {
  def eagerly: StrictMapOps[A, T, From] =
    macro CommonMapOps.eagerly[A, T, From]
  
  def lazily: NonStrictMapOps[A, T] =
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
  
  def eagerly[A : c.WeakTypeTag, T : c.WeakTypeTag, From : c.WeakTypeTag](c: Context): c.Expr[StrictMapOps[A, T, From]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "StrictMapOps"),
        deconstruct(c) :: Nil)
    } (StrictMapOpsTag[A, T, From](c))
  }
  
  def lazily[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[NonStrictMapOps[A, T]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "NonStrictMapOps"),
        deconstruct(c) :: Nil)
    } (NonStrictMapOpsTag[A, T](c))
  }
  
  private def StrictMapOpsTag[A : c.WeakTypeTag, T : c.WeakTypeTag, From : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[StrictMapOps[A, T, From]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.StrictMapOps").toType,
        weakTypeOf[A] :: weakTypeOf[T] :: weakTypeOf[From] :: Nil))
  }
  
  private def NonStrictMapOpsTag[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[NonStrictMapOps[A, T]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.NonStrictMapOps").toType,
        weakTypeOf[A] :: weakTypeOf[T] :: Nil))
  }
}
