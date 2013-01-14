/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Implicit conversions that add general and non-strictly-evaluated operations to collections.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    NonStrict
  * 
  * @groupname  General     General collection extensions
  * @groupprio  General     1
  * 
  * @groupname  NonStrict   Non-strict collection extensions
  * @groupprio  NonStrict   2
  */
class NonStrict extends General {
  /** Implicitly provides non-strictly evaluated operations for arrays.
    * @group NonStrict */
  implicit def NonStrictArrayOps[A](these: Array[A]): NonStrictArrayOps[A] =
    macro NonStrict.NonStrictArrayOps[A]
  
  /** Implicitly provides non-strictly evaluated operations for enumerators.
    * @group NonStrict */
  implicit def NonStrictEnumeratorOps[A](these: Enumerator[A]): NonStrictEnumeratorOps[A] =
    macro NonStrict.NonStrictEnumeratorOps[A]
  
  /** Implicitly provides non-strictly evaluated operations for iterators.
    * @group NonStrict */
  implicit def NonStrictIteratorOps[A](these: Iterator[A]): NonStrictIteratorOps[A] =
    macro NonStrict.NonStrictIteratorOps[A]
  
  /** Implicitly provides non-strictly evaluated operations for collections.
    * @group NonStrict */
  implicit def NonStrictCollectionOps[A](these: Collection[A]): NonStrictCollectionOps[A] =
    macro NonStrict.NonStrictCollectionOps[A]
  
  /** Implicitly provides non-strictly evaluated operations for containers.
    * @group NonStrict */
  implicit def NonStrictContainerOps[A](these: Container[A]): NonStrictContainerOps[A] =
    macro NonStrict.NonStrictContainerOps[A]
  
  /** Implicitly provides non-strictly evaluated operations for sequences.
    * @group NonStrict */
  implicit def NonStrictSeqOps[A](these: Seq[A]): NonStrictSeqOps[A] =
    macro NonStrict.NonStrictSeqOps[A]
  
  /** Implicitly provides non-strictly evaluated operations for indexes.
    * @group NonStrict */
  implicit def NonStrictIndexOps[A](these: Index[A]): NonStrictIndexOps[A] =
    macro NonStrict.NonStrictIndexOps[A]
  
  /** Implicitly provides non-strictly evaluated operations for stacks.
    * @group NonStrict */
  implicit def NonStrictStackOps[A](these: Stack[A]): NonStrictStackOps[A] =
    macro NonStrict.NonStrictStackOps[A]
  
  /** Implicitly provides non-strictly evaluated operations for sets.
    * @group NonStrict */
  implicit def NonStrictSetOps[A](these: Set[A]): NonStrictSetOps[A] =
    macro NonStrict.NonStrictSetOps[A]
  
  /** Implicitly provides non-strictly evaluated operations for maps.
    * @group NonStrict */
  implicit def NonStrictMapOps[A, T](these: Map[A, T]): NonStrictMapOps[A, T] =
    macro NonStrict.NonStrictMapOps[A, T]
}

private[sequential] object NonStrict {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def NonStrictArrayOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Array[A]])
    : c.Expr[NonStrictArrayOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictArrayOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictArrayOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(NonStrictArrayOpsType, these.tree))(WeakTypeTag(NonStrictArrayOpsType))
  }
  
  def NonStrictEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[NonStrictEnumeratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictEnumeratorOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictEnumeratorOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(NonStrictEnumeratorOpsType, these.tree))(WeakTypeTag(NonStrictEnumeratorOpsType))
  }
  
  def NonStrictIteratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[NonStrictIteratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictIteratorOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictIteratorOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(NonStrictIteratorOpsType, these.tree))(WeakTypeTag(NonStrictIteratorOpsType))
  }
  
  def NonStrictCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[NonStrictCollectionOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictCollectionOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictCollectionOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(NonStrictCollectionOpsType, these.tree))(WeakTypeTag(NonStrictCollectionOpsType))
  }
  
  def NonStrictContainerOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[NonStrictContainerOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictContainerOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictContainerOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(NonStrictContainerOpsType, these.tree))(WeakTypeTag(NonStrictContainerOpsType))
  }
  
  def NonStrictSeqOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[NonStrictSeqOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictSeqOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictSeqOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(NonStrictSeqOpsType, these.tree))(WeakTypeTag(NonStrictSeqOpsType))
  }
  
  def NonStrictIndexOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Index[A]])
    : c.Expr[NonStrictIndexOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictIndexOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictIndexOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(NonStrictIndexOpsType, these.tree))(WeakTypeTag(NonStrictIndexOpsType))
  }
  
  def NonStrictStackOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Stack[A]])
    : c.Expr[NonStrictStackOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictStackOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictStackOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(NonStrictStackOpsType, these.tree))(WeakTypeTag(NonStrictStackOpsType))
  }
  
  def NonStrictSetOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[NonStrictSetOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictSetOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictSetOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(NonStrictSetOpsType, these.tree))(WeakTypeTag(NonStrictSetOpsType))
  }
  
  def NonStrictMapOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[NonStrictMapOps[A, T]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val NonStrictMapOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.NonStrictMapOps").toType,
        weakTypeOf[A] :: weakTypeOf[T] :: Nil)
    Expr(New(NonStrictMapOpsType, these.tree))(WeakTypeTag(NonStrictMapOpsType))
  }
}
