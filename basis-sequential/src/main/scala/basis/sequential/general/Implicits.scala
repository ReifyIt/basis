/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package general

import basis.collections._

private[sequential] trait Implicits {
  implicit final def GeneralEnumeratorOps[A](these: Enumerator[A]): EnumeratorOps[A] =
    macro Implicits.GeneralEnumeratorOps[A]
  
  implicit final def GeneralIteratorOps[A](these: Iterator[A]): IteratorOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralCollectionOps[A](these: Collection[A]): CollectionOps[A] =
    macro Implicits.GeneralCollectionOps[A]
  
  implicit final def GeneralContainerOps[A](these: Container[A]): ContainerOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralSeqOps[A](these: Seq[A]): SeqOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralIndexOps[A](these: Index[A]): IndexOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralStackOps[A](these: Stack[A]): StackOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralSetOps[A](these: Set[A]): SetOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralMapOps[A, T](these: Map[A, T]): MapOps[A, T] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}

private[sequential] object Implicits {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def GeneralEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[EnumeratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val EnumeratorOpsTag =
      WeakTypeTag[EnumeratorOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.general.EnumeratorOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(EnumeratorOpsTag.tpe, these.tree))(EnumeratorOpsTag)
  }
  
  def GeneralCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[CollectionOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val CollectionOpsTag =
      WeakTypeTag[CollectionOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.general.CollectionOps").toType,
          weakTypeOf[A] :: Nil))
    Expr(New(CollectionOpsTag.tpe, these.tree))(CollectionOpsTag)
  }
}
