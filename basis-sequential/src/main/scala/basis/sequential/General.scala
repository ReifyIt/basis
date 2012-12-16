/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential

import basis.collections._

class General {
  implicit final def GeneralEnumeratorOps[A](these: Enumerator[A]): GeneralEnumeratorOps[A] =
    macro General.GeneralEnumeratorOps[A]
  
  implicit final def GeneralIteratorOps[A](these: Iterator[A]): GeneralIteratorOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralCollectionOps[A](these: Collection[A]): GeneralCollectionOps[A] =
    macro General.GeneralCollectionOps[A]
  
  implicit final def GeneralContainerOps[A](these: Container[A]): GeneralContainerOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralSeqOps[A](these: Seq[A]): GeneralSeqOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralIndexOps[A](these: Index[A]): GeneralIndexOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralStackOps[A](these: Stack[A]): GeneralStackOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralSetOps[A](these: Set[A]): GeneralSetOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def GeneralMapOps[A, T](these: Map[A, T]): GeneralMapOps[A, T] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}

private[sequential] object General {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def GeneralEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[GeneralEnumeratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralEnumeratorOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralEnumeratorOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralEnumeratorOpsType, these.tree))(WeakTypeTag(GeneralEnumeratorOpsType))
  }
  
  def GeneralCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[GeneralCollectionOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralCollectionOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralCollectionOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralCollectionOpsType, these.tree))(WeakTypeTag(GeneralCollectionOpsType))
  }
}
