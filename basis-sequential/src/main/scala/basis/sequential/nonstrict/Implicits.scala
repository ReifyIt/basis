/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package nonstrict

import basis.collections._

private[sequential] trait Implicits {
  implicit final def NonStrictEnumeratorOps[A](these: Enumerator[A]): EnumeratorOps[A] =
    macro Implicits.NonStrictEnumeratorOps[A]
  
  implicit final def NonStrictIteratorOps[A](these: Iterator[A]): IteratorOps[A] =
    macro Implicits.NonStrictIteratorOps[A]
  
  implicit final def NonStrictCollectionOps[A](these: Collection[A]): CollectionOps[A] =
    macro Implicits.NonStrictCollectionOps[A]
  
  implicit final def NonStrictContainerOps[A](these: Container[A]): ContainerOps[A] =
    macro Implicits.NonStrictContainerOps[A]
  
  implicit final def NonStrictSeqOps[A](these: Seq[A]): SeqOps[A] =
    macro Implicits.NonStrictSeqOps[A]
  
  implicit final def NonStrictIndexOps[A](these: Index[A]): IndexOps[A] =
    macro Implicits.NonStrictIndexOps[A]
  
  implicit final def NonStrictStackOps[A](these: Stack[A]): StackOps[A] =
    macro Implicits.NonStrictStackOps[A]
  
  implicit final def NonStrictSetOps[A](these: Set[A]): SetOps[A] =
    macro Implicits.NonStrictSetOps[A]
  
  implicit final def NonStrictMapOps[A, T](these: Map[A, T]): MapOps[A, T] =
    macro Implicits.NonStrictMapOps[A, T]
}

private[sequential] object Implicits {
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
  
  def NonStrictStackOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Stack[A]])
    : c.Expr[StackOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StackOpsTag =
      WeakTypeTag[StackOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.StackOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(StackOpsTag.tpe, these.tree))(StackOpsTag)
  }
  
  def NonStrictIndexOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Index[A]])
    : c.Expr[IndexOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val IndexOpsTag =
      WeakTypeTag[IndexOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.nonstrict.IndexOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(IndexOpsTag.tpe, these.tree))(IndexOpsTag)
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
