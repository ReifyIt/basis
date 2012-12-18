/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential

import basis.collections._

/** General map operations.
  * 
  * @groupprio  Traversing    -4
  * @groupprio  Reducing      -3
  * @groupprio  Querying      -2
  * @groupprio  Transforming  -1
  */
final class GeneralMapOps[+A, +T](these: Map[A, T]) {
  /** Returns a strict operations interface to this map.
    * @group Transforming */
  def eagerly: StrictMapOps[A, T, Map[A, T]] =
    macro GeneralMapOps.eagerly[A, T]
  
  /** Returns a non-strict operations interface to this map.
    * @group Transforming */
  def lazily: NonStrictMapOps[A, T] =
    macro GeneralMapOps.lazily[A, T]
}

private[sequential] object GeneralMapOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[Map[A, T]] = {
    import c.{Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val Apply(_, map :: Nil) = prefix.tree
    val MapType =
      appliedType(
        mirror.staticClass("basis.collections.Map").toType,
        weakTypeOf[A] :: weakTypeOf[T] :: Nil)
    Expr(typeCheck(map, MapType))(WeakTypeTag(MapType))
  }
  
  def eagerly[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[StrictMapOps[A, T, Map[A, T]]] =
    Strict.StrictMapOps[A, T](c)(unApply[A, T](c))
  
  def lazily[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[NonStrictMapOps[A, T]] =
    NonStrict.NonStrictMapOps[A, T](c)(unApply[A, T](c))
}
