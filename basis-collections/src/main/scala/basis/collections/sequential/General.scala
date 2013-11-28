//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

class General {
  /** Implicitly adds general operations to arrays.
    * @group General */
  implicit def ArrayToGeneralOps[A](these: Array[A]): GeneralArrayOps[A] =
    macro General.ArrayToGeneralOps[A]

  /** Implicitly adds general operations to traversers.
    * @group General */
  implicit def TraverserToGeneralOps[A](these: Traverser[A]): GeneralTraverserOps[A] =
    macro General.TraverserToGeneralOps[A]

  /** Implicitly adds general operations to iterators.
    * @group General */
  implicit def IteratorToGeneralOps[A](these: Iterator[A]): GeneralIteratorOps[A] =
    macro General.IteratorToGeneralOps[A]

  /** Implicitly adds general operations to collections.
    * @group General */
  implicit def CollectionToGeneralOps[A](these: Collection[A]): GeneralCollectionOps[A] =
    macro General.CollectionToGeneralOps[A]

  /** Implicitly adds general operations to containers.
    * @group General */
  implicit def ContainerToGeneralOps[A](these: Container[A]): GeneralContainerOps[A] =
    macro General.ContainerToGeneralOps[A]

  /** Implicitly adds general operations to sequences.
    * @group General */
  implicit def SeqToGeneralOps[A](these: Seq[A]): GeneralSeqOps[A] =
    macro General.SeqToGeneralOps[A]

  /** Implicitly adds general operations to indexed sequences.
    * @group General */
  implicit def IndexedSeqToGeneralOps[A](these: IndexedSeq[A]): GeneralIndexedSeqOps[A] =
    macro General.IndexedSeqToGeneralOps[A]

  /** Implicitly adds general operations to linear sequences.
    * @group General */
  implicit def LinearSeqToGeneralOps[A](these: LinearSeq[A]): GeneralLinearSeqOps[A] =
    macro General.LinearSeqToGeneralOps[A]

  /** Implicitly adds general operations to bilinear sequences.
    * @group General */
  implicit def BilinearSeqToGeneralOps[A](these: BilinearSeq[A]): GeneralSeqOps[A] =
    macro General.SeqToGeneralOps[A]

  /** Implicitly adds general operations to sets.
    * @group General */
  implicit def SetToGeneralOps[A](these: Set[A]): GeneralSetOps[A] =
    macro General.SetToGeneralOps[A]

  /** Implicitly adds general operations to maps.
    * @group General */
  implicit def MapToGeneralOps[A, T](these: Map[A, T]): GeneralMapOps[A, T] =
    macro General.MapToGeneralOps[A, T]
}

private[collections] object General {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  def ArrayToGeneralOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Array[A]])
    : c.Expr[GeneralArrayOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralArrayOpsATag =
      WeakTypeTag[GeneralArrayOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralArrayOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralArrayOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralArrayOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def TraverserToGeneralOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Traverser[A]])
    : c.Expr[GeneralTraverserOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralTraverserOpsATag =
      WeakTypeTag[GeneralTraverserOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralTraverserOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralTraverserOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralTraverserOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def IteratorToGeneralOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[GeneralIteratorOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralIteratorOpsATag =
      WeakTypeTag[GeneralIteratorOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralIteratorOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralIteratorOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralIteratorOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def CollectionToGeneralOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[GeneralCollectionOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralCollectionOpsATag =
      WeakTypeTag[GeneralCollectionOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralCollectionOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralCollectionOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralCollectionOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def ContainerToGeneralOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[GeneralContainerOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralContainerOpsATag =
      WeakTypeTag[GeneralContainerOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralContainerOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralContainerOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralContainerOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def SeqToGeneralOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[GeneralSeqOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralSeqOpsATag =
      WeakTypeTag[GeneralSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralSeqOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralSeqOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def IndexedSeqToGeneralOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[IndexedSeq[A]])
    : c.Expr[GeneralIndexedSeqOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralIndexedSeqOpsATag =
      WeakTypeTag[GeneralIndexedSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralIndexedSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralIndexedSeqOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralIndexedSeqOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def LinearSeqToGeneralOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[LinearSeq[A]])
    : c.Expr[GeneralLinearSeqOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralLinearSeqOpsATag =
      WeakTypeTag[GeneralLinearSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralLinearSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralLinearSeqOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralLinearSeqOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def SetToGeneralOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[GeneralSetOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralSetOpsATag =
      WeakTypeTag[GeneralSetOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralSetOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[GeneralSetOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralSetOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def MapToGeneralOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[GeneralMapOps[A, T]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val GeneralMapOpsATTag =
      WeakTypeTag[GeneralMapOps[A, T]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.GeneralMapOps").toType,
          weakTypeOf[A] :: weakTypeOf[T] :: Nil))
    Expr[GeneralMapOps[A, T]](
      Apply(
        Select(New(TypeTree(weakTypeOf[GeneralMapOps[A, T]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
}
