//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

class NonStrict extends General {
  /** Implicitly adds non-strictly evaluated operations to arrays.
    * @group NonStrict */
  implicit def ArrayToNonStrictOps[A](these: Array[A]): NonStrictArrayOps[A] =
    macro NonStrict.ArrayToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to traversers.
    * @group NonStrict */
  implicit def TraverserToNonStrictOps[A](these: Traverser[A]): NonStrictTraverserOps[A] =
    macro NonStrict.TraverserToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to iterators.
    * @group NonStrict */
  implicit def IteratorToNonStrictOps[A](these: Iterator[A]): NonStrictIteratorOps[A] =
    macro NonStrict.IteratorToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to collections.
    * @group NonStrict */
  implicit def CollectionToNonStrictOps[A](these: Collection[A]): NonStrictCollectionOps[A] =
    macro NonStrict.CollectionToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to containers.
    * @group NonStrict */
  implicit def ContainerToNonStrictOps[A](these: Container[A]): NonStrictContainerOps[A] =
    macro NonStrict.ContainerToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to sequences.
    * @group NonStrict */
  implicit def SeqToNonStrictOps[A](these: Seq[A]): NonStrictSeqOps[A] =
    macro NonStrict.SeqToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to indexed sequences.
    * @group NonStrict */
  implicit def IndexedSeqToNonStrictOps[A](these: IndexedSeq[A]): NonStrictIndexedSeqOps[A] =
    macro NonStrict.IndexedSeqToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to linear sequences.
    * @group NonStrict */
  implicit def LinearSeqToNonStrictOps[A](these: LinearSeq[A]): NonStrictLinearSeqOps[A] =
    macro NonStrict.LinearSeqToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to bilinear sequences.
    * @group NonStrict */
  implicit def BilinearSeqToNonStrictOps[A](these: BilinearSeq[A]): NonStrictSeqOps[A] =
    macro NonStrict.SeqToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to sets.
    * @group NonStrict */
  implicit def SetToNonStrictOps[A](these: Set[A]): NonStrictSetOps[A] =
    macro NonStrict.SetToNonStrictOps[A]

  /** Implicitly adds non-strictly evaluated operations to maps.
    * @group NonStrict */
  implicit def MapToNonStrictOps[A, T](these: Map[A, T]): NonStrictMapOps[A, T] =
    macro NonStrict.MapToNonStrictOps[A, T]
}

private[collections] object NonStrict {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  def ArrayToNonStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Array[A]])
    : c.Expr[NonStrictArrayOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictArrayOpsATag =
      WeakTypeTag[NonStrictArrayOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictArrayOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictArrayOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictArrayOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def TraverserToNonStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Traverser[A]])
    : c.Expr[NonStrictTraverserOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictTraverserOpsATag =
      WeakTypeTag[NonStrictTraverserOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictTraverserOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictTraverserOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictTraverserOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def IteratorToNonStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[NonStrictIteratorOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictIteratorOpsATag =
      WeakTypeTag[NonStrictIteratorOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictIteratorOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictIteratorOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictIteratorOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def CollectionToNonStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[NonStrictCollectionOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictCollectionOpsATag =
      WeakTypeTag[NonStrictCollectionOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictCollectionOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictCollectionOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictCollectionOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def ContainerToNonStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[NonStrictContainerOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictContainerOpsATag =
      WeakTypeTag[NonStrictContainerOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictContainerOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictContainerOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictContainerOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def SeqToNonStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[NonStrictSeqOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictSeqOpsATag =
      WeakTypeTag[NonStrictSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictSeqOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictSeqOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def IndexedSeqToNonStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[IndexedSeq[A]])
    : c.Expr[NonStrictIndexedSeqOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictIndexedSeqOpsATag =
      WeakTypeTag[NonStrictIndexedSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictIndexedSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictIndexedSeqOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictIndexedSeqOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def LinearSeqToNonStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[LinearSeq[A]])
    : c.Expr[NonStrictLinearSeqOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictLinearSeqOpsATag =
      WeakTypeTag[NonStrictLinearSeqOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictLinearSeqOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictLinearSeqOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictLinearSeqOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def SetToNonStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[NonStrictSetOps[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictSetOpsATag =
      WeakTypeTag[NonStrictSetOps[A]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictSetOps").toType,
          weakTypeOf[A] :: Nil))
    Expr[NonStrictSetOps[A]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictSetOps[A]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def MapToNonStrictOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[NonStrictMapOps[A, T]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val NonStrictMapOpsATTag =
      WeakTypeTag[NonStrictMapOps[A, T]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.NonStrictMapOps").toType,
          weakTypeOf[A] :: weakTypeOf[T] :: Nil))
    Expr[NonStrictMapOps[A, T]](
      Apply(
        Select(New(TypeTree(weakTypeOf[NonStrictMapOps[A, T]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
}
