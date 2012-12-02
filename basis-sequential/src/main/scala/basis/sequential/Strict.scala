/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential

import basis.collections._
import basis.collections.traversable._
import basis.sequential.strict._

trait Strict {
  implicit final def StrictEnumeratorOps[A](these: Enumerator[A]): EnumeratorOps[A, these.Parent] =
    macro Strict.StrictEnumeratorOps[A]
  
  implicit final def StrictIteratorOps[A](these: Iterator[A]): IteratorOps[A, these.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictCollectionOps[A](these: Collection[A]): CollectionOps[A, these.Parent] =
    macro Strict.StrictCollectionOps[A]
  
  implicit final def StrictContainerOps[A](these: Container[A]): ContainerOps[A, these.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictSeqOps[A](these: Seq[A]): SeqOps[A, these.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictLinearSeqOps[A](these: LinearSeq[A]): LinearSeqOps[A, these.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictIndexedSeqOps[A](these: IndexedSeq[A]): IndexedSeqOps[A, these.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictSetOps[A](these: Set[A]): SetOps[A, these.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictMapOps[A, T](these: Map[A, T]): MapOps[A, T, these.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}

private[sequential] object Strict {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def StrictEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[EnumeratorOps[A, these.value.Parent]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val EnumeratorOpsTag =
      WeakTypeTag[EnumeratorOps[A, these.value.Parent]](
        appliedType(
          mirror.staticClass("basis.sequential.strict.EnumeratorOps").toType,
          weakTypeOf[A] :: ParentType(c)(these) :: Nil))
    Expr(New(EnumeratorOpsTag.tpe, these.tree))(EnumeratorOpsTag)
  }
  
  def StrictCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[CollectionOps[A, these.value.Parent]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val CollectionOpsTag =
      WeakTypeTag[CollectionOps[A, these.value.Parent]](
        appliedType(
          mirror.staticClass("basis.sequential.strict.CollectionOps").toType,
          weakTypeOf[A] :: ParentType(c)(these) :: Nil))
    Expr(New(CollectionOpsTag.tpe, these.tree))(CollectionOpsTag)
  }
  
  private def ParentType[A : c.WeakTypeTag](c: Context)(these: c.Expr[Family[A]]): c.Type = {
    import c.universe._
    val TheseType = these.tree.symbol match {
      case symbol: TermSymbol if symbol.isStable => singleType(NoPrefix, symbol)
      case _ => these.actualType
    }
    val FamilyType = c.mirror.staticClass("basis.collections.Family").toType
    typeRef(TheseType, FamilyType.member(newTypeName("Parent")), Nil)
  }
}
