/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential

import basis.collections._
import basis.collections.traversable._
import basis.sequential.nonstrict._

private[sequential] trait NonStrict {
  implicit final def NonStrictEnumeratorOps[A](these: Enumerator[A]): EnumeratorOps[A] =
    macro NonStrict.NonStrictEnumeratorOps[A]
  
  implicit final def NonStrictIteratorOps[A](these: Iterator[A]): IteratorOps[A] =
    macro NonStrict.NonStrictIteratorOps[A]
  
  implicit final def NonStrictCollectionOps[A](these: Collection[A]): CollectionOps[A] =
    macro NonStrict.NonStrictCollectionOps[A]
  
  implicit final def NonStrictContainerOps[A](these: Container[A]): ContainerOps[A] =
    macro NonStrict.NonStrictContainerOps[A]
  
  implicit final def NonStrictSeqOps[A](these: Seq[A]): SeqOps[A] =
    macro NonStrict.NonStrictSeqOps[A]
  
  implicit final def NonStrictLinearSeqOps[A](these: LinearSeq[A]): LinearSeqOps[A] =
    macro NonStrict.NonStrictLinearSeqOps[A]
  
  implicit final def NonStrictIndexedSeqOps[A](these: IndexedSeq[A]): IndexedSeqOps[A] =
    macro NonStrict.NonStrictIndexedSeqOps[A]
  
  implicit final def NonStrictSetOps[A](these: Set[A]): SetOps[A] =
    macro NonStrict.NonStrictSetOps[A]
  
  implicit final def NonStrictMapOps[A, T](these: Map[A, T]): MapOps[A, T] =
    macro NonStrict.NonStrictMapOps[A, T]
}

private[sequential] object NonStrict {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def NonStrictEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[EnumeratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val EnumeratorOpsTag =
      WeakTypeTag[EnumeratorOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.EnumeratorOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(EnumeratorOpsTag.tpe, these.tree))(EnumeratorOpsTag)
  }
  
  def NonStrictIteratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[IteratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val IteratorOpsTag =
      WeakTypeTag[IteratorOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.IteratorOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(IteratorOpsTag.tpe, these.tree))(IteratorOpsTag)
  }
  
  def NonStrictCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[CollectionOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val CollectionOpsTag =
      WeakTypeTag[CollectionOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.CollectionOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(CollectionOpsTag.tpe, these.tree))(CollectionOpsTag)
  }
  
  def NonStrictContainerOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[ContainerOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val ContainerOpsTag =
      WeakTypeTag[ContainerOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.ContainerOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(ContainerOpsTag.tpe, these.tree))(ContainerOpsTag)
  }
  
  def NonStrictSeqOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[SeqOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val SeqOpsTag =
      WeakTypeTag[SeqOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.SeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(SeqOpsTag.tpe, these.tree))(SeqOpsTag)
  }
  
  def NonStrictLinearSeqOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[LinearSeq[A]])
    : c.Expr[LinearSeqOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val LinearSeqOpsTag =
      WeakTypeTag[LinearSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.LinearSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(LinearSeqOpsTag.tpe, these.tree))(LinearSeqOpsTag)
  }
  
  def NonStrictIndexedSeqOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[IndexedSeq[A]])
    : c.Expr[IndexedSeqOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val IndexedSeqOpsTag =
      WeakTypeTag[IndexedSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.IndexedSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(IndexedSeqOpsTag.tpe, these.tree))(IndexedSeqOpsTag)
  }
  
  def NonStrictSetOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[SetOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val SetOpsTag =
      WeakTypeTag[SetOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.SetOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(SetOpsTag.tpe, these.tree))(SetOpsTag)
  }
  
  def NonStrictMapOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[MapOps[A, T]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val MapOpsTag =
      WeakTypeTag[MapOps[A, T]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.MapOps").toType,
          weakTypeOf[A] :: weakTypeOf[T] :: Nil))
    Expr(New(MapOpsTag.tpe, these.tree))(MapOpsTag)
  }
}
