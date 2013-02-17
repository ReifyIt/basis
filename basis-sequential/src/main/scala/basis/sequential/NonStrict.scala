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
    implicit val NonStrictArrayOpsATag =
      WeakTypeTag[NonStrictArrayOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictArrayOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictArrayOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictArrayOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def NonStrictEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[NonStrictEnumeratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val NonStrictEnumeratorOpsATag =
      WeakTypeTag[NonStrictEnumeratorOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictEnumeratorOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictEnumeratorOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictEnumeratorOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def NonStrictIteratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[NonStrictIteratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val NonStrictIteratorOpsATag =
      WeakTypeTag[NonStrictIteratorOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictIteratorOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictIteratorOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictIteratorOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def NonStrictCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[NonStrictCollectionOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val NonStrictCollectionOpsATag =
      WeakTypeTag[NonStrictCollectionOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictCollectionOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictCollectionOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictCollectionOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def NonStrictContainerOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[NonStrictContainerOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val NonStrictContainerOpsATag =
      WeakTypeTag[NonStrictContainerOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictContainerOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictContainerOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictContainerOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def NonStrictSeqOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[NonStrictSeqOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val NonStrictSeqOpsATag =
      WeakTypeTag[NonStrictSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictSeqOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictSeqOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def NonStrictIndexOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Index[A]])
    : c.Expr[NonStrictIndexOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val NonStrictIndexOpsATag =
      WeakTypeTag[NonStrictIndexOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictIndexOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictIndexOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictIndexOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def NonStrictStackOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Stack[A]])
    : c.Expr[NonStrictStackOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val NonStrictStackOpsATag =
      WeakTypeTag[NonStrictStackOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictStackOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictStackOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictStackOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def NonStrictSetOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[NonStrictSetOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val NonStrictSetOpsATag =
      WeakTypeTag[NonStrictSetOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictSetOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictSetOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictSetOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def NonStrictMapOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[NonStrictMapOps[A, T]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val NonStrictMapOpsATTag =
      WeakTypeTag[NonStrictMapOps[A, T]](
        appliedType(
          mirror.staticClass("basis.sequential.NonStrictMapOps").toType,
          weakTypeOf[A] :: weakTypeOf[T] :: Nil))
    Expr[NonStrictMapOps[A, T]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictMapOps[A, T]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
}
