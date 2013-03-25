/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Implicit conversions that add general operations to collections.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    General
  * 
  * @groupname  General   General collection extensions
  * @groupprio  General   1
  */
class General {
  /** Implicitly provides general operations for arrays.
    * @group General */
  implicit def GeneralArrayOps[A](these: Array[A]): GeneralArrayOps[A] =
    macro General.GeneralArrayOps[A]
  
  /** Implicitly provides general operations for enumerators.
    * @group General */
  implicit def GeneralEnumeratorOps[A](these: Enumerator[A]): GeneralEnumeratorOps[A] =
    macro General.GeneralEnumeratorOps[A]
  
  /** Implicitly provides general operations for iterators.
    * @group General */
  implicit def GeneralIteratorOps[A](these: Iterator[A]): GeneralIteratorOps[A] =
    macro General.GeneralIteratorOps[A]
  
  /** Implicitly provides general operations for collections.
    * @group General */
  implicit def GeneralCollectionOps[A](these: Collection[A]): GeneralCollectionOps[A] =
    macro General.GeneralCollectionOps[A]
  
  /** Implicitly provides general operations for containers.
    * @group General */
  implicit def GeneralContainerOps[A](these: Container[A]): GeneralContainerOps[A] =
    macro General.GeneralContainerOps[A]
  
  /** Implicitly provides general operations for sequences.
    * @group General */
  implicit def GeneralSeqOps[A](these: Seq[A]): GeneralSeqOps[A] =
    macro General.GeneralSeqOps[A]
  
  /** Implicitly provides general operations for indexes.
    * @group General */
  implicit def GeneralIndexOps[A](these: Index[A]): GeneralIndexOps[A] =
    macro General.GeneralIndexOps[A]
  
  /** Implicitly provides general operations for links.
    * @group General */
  implicit def GeneralLinkOps[A](these: Link[A]): GeneralLinkOps[A] =
    macro General.GeneralLinkOps[A]
  
  /** Implicitly provides general operations for sets.
    * @group General */
  implicit def GeneralSetOps[A](these: Set[A]): GeneralSetOps[A] =
    macro General.GeneralSetOps[A]
  
  /** Implicitly provides general operations for maps.
    * @group General */
  implicit def GeneralMapOps[A, T](these: Map[A, T]): GeneralMapOps[A, T] =
    macro General.GeneralMapOps[A, T]
}

private[sequential] object General {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def GeneralArrayOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Array[A]])
    : c.Expr[GeneralArrayOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralArrayOpsATag =
      WeakTypeTag[GeneralArrayOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralArrayOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralArrayOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralArrayOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def GeneralEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[GeneralEnumeratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralEnumeratorOpsATag =
      WeakTypeTag[GeneralEnumeratorOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralEnumeratorOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralEnumeratorOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralEnumeratorOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def GeneralIteratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[GeneralIteratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralIteratorOpsATag =
      WeakTypeTag[GeneralIteratorOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralIteratorOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralIteratorOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralIteratorOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def GeneralCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[GeneralCollectionOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralCollectionOpsATag =
      WeakTypeTag[GeneralCollectionOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralCollectionOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralCollectionOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralCollectionOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def GeneralContainerOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[GeneralContainerOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralContainerOpsATag =
      WeakTypeTag[GeneralContainerOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralContainerOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralContainerOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralContainerOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def GeneralSeqOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[GeneralSeqOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralSeqOpsATag =
      WeakTypeTag[GeneralSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralSeqOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralSeqOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def GeneralIndexOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Index[A]])
    : c.Expr[GeneralIndexOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralIndexOpsATag =
      WeakTypeTag[GeneralIndexOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralIndexOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralIndexOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralIndexOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def GeneralLinkOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Link[A]])
    : c.Expr[GeneralLinkOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralLinkOpsATag =
      WeakTypeTag[GeneralLinkOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralLinkOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralLinkOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralLinkOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def GeneralSetOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[GeneralSetOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralSetOpsATag =
      WeakTypeTag[GeneralSetOps[A]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralSetOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralSetOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralSetOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def GeneralMapOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[GeneralMapOps[A, T]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val GeneralMapOpsATTag =
      WeakTypeTag[GeneralMapOps[A, T]](
        appliedType(
          mirror.staticClass("basis.sequential.GeneralMapOps").toType,
          weakTypeOf[A] :: weakTypeOf[T] :: Nil))
    Expr[GeneralMapOps[A, T]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralMapOps[A, T]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
}
