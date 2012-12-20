/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
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
  /** Sequentially applies a function to each entry of this map.
    * 
    * @param  f   the function to apply to each entry.
    * @group  Traversing
    */
  def foreach[U](f: ((A, T)) => U): Unit =
    macro GeneralContainerOps.foreach[(A, T), U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity entry and all entries of this map.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: (A, T)](z: B)(op: (B, B) => B): B =
    macro GeneralContainerOps.foldLeft[(A, T), B]
  
  /** Returns the repeated application of an associative binary operator
    * between all entries of this non-empty map.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: (A, T)](op: (B, B) => B): B =
    macro GeneralContainerOps.reduceLeft[(A, T), B]
  
  /** Returns the repeated application of an associative binary operator
    * between all entries of this map.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if this map is empty.
    * @group  Reducing
    */
  def reduceOption[B >: (A, T)](op: (B, B) => B): Option[B] =
    macro GeneralContainerOps.reduceLeftOption[(A, T), B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all entries of this map.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, (A, T)) => B): B =
    macro GeneralContainerOps.foldLeft[(A, T), B]
  
  /** Returns the left-to-right application of a binary operator between
    * all entries of this non-empty map.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: (A, T)](op: (B, (A, T)) => B): B =
    macro GeneralContainerOps.reduceLeft[(A, T), B]
  
  /** Returns the left-to-right application of a binary operator between
    * all entries of this map.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if this map is empty.
    * @group  Reducing
    */
  def reduceLeftOption[B >: (A, T)](op: (B, (A, T)) => B): Option[B] =
    macro GeneralContainerOps.reduceLeftOption[(A, T), B]
  
  /** Returns the first entry of this map that satisfies a predicate.
    * 
    * @param  p   the predicate to test entries against.
    * @return some found entry, or none if no entry satisfies `p`.
    * @group  Querying
    */
  def find(p: ((A, T)) => Boolean): Option[(A, T)] =
    macro GeneralContainerOps.find[(A, T)]
  
  /** Returns `true` if a predicate holds for all entries of this map.
    * 
    * @param  p   the predicate to test entries against.
    * @return `true` if all entries satisfy `p`, otherwise `false`.
    * @group  Querying
    */
  def forall(p: ((A, T)) => Boolean): Boolean =
    macro GeneralContainerOps.forall[(A, T)]
  
  /** Returns `true` if a predicate holds for some entry of this map.
    * 
    * @param  p   the predicate to test entries against.
    * @return `true` if any entry satisfies `p`, otherwise `false`.
    * @group  Querying
    */
  def exists(p: ((A, T)) => Boolean): Boolean =
    macro GeneralContainerOps.exists[(A, T)]
  
  /** Returns the number of entries in this map that satisfy a predicate.
    * 
    * @param  p   the predicate to test entries against.
    * @return the number of entries satisfying `p`.
    * @group  Querying
    */
  def count(p: ((A, T)) => Boolean): Int =
    macro GeneralContainerOps.count[(A, T)]
  
  /** Returns the application of a partial function to the first entry
    * of this map for which the function is defined.
    * 
    * @param  q   the partial function to test entries against and to apply
    *             to the first found entry.
    * @return some found and mapped entry, or none if no entry applies to `q`.
    * @group  Querying
    */
  def choose[B](q: PartialFunction[(A, T), B]): Option[B] =
    macro GeneralContainerOps.choose[(A, T), B]
  
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
