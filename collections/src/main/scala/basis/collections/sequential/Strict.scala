//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

class Strict extends General {
  /** Implicitly adds strictly evaluated operations to arrays.
    * @group Strict */
  implicit def ArrayToStrictOps[A](these: Array[A]): StrictArrayOps[A, Array[_]] =
    macro Strict.ArrayToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to traversers.
    * @group Strict */
  implicit def TraverserToStrictOps[A](these: Traverser[A]): StrictTraverserOps[A, these.Family] =
    macro Strict.TraverserToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to iterators.
    * @group Strict */
  implicit def IteratorToStrictOps[A](these: Iterator[A]): StrictIteratorOps[A, these.Family] =
    macro Strict.IteratorToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to collections.
    * @group Strict */
  implicit def CollectionToStrictOps[A](these: Collection[A]): StrictCollectionOps[A, these.Family] =
    macro Strict.CollectionToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to containers.
    * @group Strict */
  implicit def ContainerToStrictOps[A](these: Container[A]): StrictContainerOps[A, these.Family] =
    macro Strict.ContainerToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to sequences.
    * @group Strict */
  implicit def SeqToStringOps[A](these: Seq[A]): StrictSeqOps[A, these.Family] =
    macro Strict.SeqToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to indexed sequences.
    * @group Strict */
  implicit def IndexedSeqToStrictOps[A](these: IndexedSeq[A]): StrictIndexedSeqOps[A, these.Family] =
    macro Strict.IndexedSeqToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to linear sequences.
    * @group Strict */
  implicit def LinearSeqToStrictOps[A](these: LinearSeq[A]): StrictLinearSeqOps[A, these.Family] =
    macro Strict.LinearSeqToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to bilinear sequences.
    * @group Strict */
  implicit def BilinearSeqToStrictOps[A](these: BilinearSeq[A]): StrictSeqOps[A, these.Family] =
    macro Strict.SeqToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to sets.
    * @group Strict */
  implicit def SetToStrictOps[A](these: Set[A]): StrictSetOps[A, these.Family] =
    macro Strict.SetToStrictOps[A]

  /** Implicitly adds strictly evaluated operations to maps.
    * @group Strict */
  implicit def MapToStrictOps[A, T](these: Map[A, T]): StrictMapOps[A, T, these.Family] =
    macro Strict.MapToStrictOps[A, T]
}

private[collections] object Strict {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  def ArrayToStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Array[A]])
    : c.Expr[StrictArrayOps[A, Array[_]]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictArrayOpsATag =
      WeakTypeTag[StrictArrayOps[A, Array[_]]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictArrayOps").toType,
          weakTypeOf[A] :: appliedType(mirror.staticClass("scala.Array").toType, WildcardType :: Nil) :: Nil))
    Expr[StrictArrayOps[A, Array[_]]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictArrayOps[A, Array[_]]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def TraverserToStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Traverser[A]])
    : c.Expr[StrictTraverserOps[A, these.value.Family]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictTraverserOpsATag =
      WeakTypeTag[StrictTraverserOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictTraverserOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictTraverserOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictTraverserOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def IteratorToStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[StrictIteratorOps[A, these.value.Family]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictIteratorOpsATag =
      WeakTypeTag[StrictIteratorOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictIteratorOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictIteratorOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictIteratorOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def CollectionToStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[StrictCollectionOps[A, these.value.Family]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictCollectionOpsATag =
      WeakTypeTag[StrictCollectionOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictCollectionOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictCollectionOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictCollectionOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def ContainerToStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[StrictContainerOps[A, these.value.Family]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictContainerOpsATag =
      WeakTypeTag[StrictContainerOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictContainerOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictContainerOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictContainerOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def SeqToStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[StrictSeqOps[A, these.value.Family]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictSeqOpsATag =
      WeakTypeTag[StrictSeqOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictSeqOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictSeqOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictSeqOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def IndexedSeqToStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[IndexedSeq[A]])
    : c.Expr[StrictIndexedSeqOps[A, these.value.Family]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictIndexedSeqOpsATag =
      WeakTypeTag[StrictIndexedSeqOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictIndexedSeqOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictIndexedSeqOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictIndexedSeqOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def LinearSeqToStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[LinearSeq[A]])
    : c.Expr[StrictLinearSeqOps[A, these.value.Family]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictLinearSeqOpsATag =
      WeakTypeTag[StrictLinearSeqOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictLinearSeqOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictLinearSeqOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictLinearSeqOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def SetToStrictOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[StrictSetOps[A, these.value.Family]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictSetOpsATag =
      WeakTypeTag[StrictSetOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictSetOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictSetOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictSetOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  def MapToStrictOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[StrictMapOps[A, T, these.value.Family]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val StrictMapOpsATTag =
      WeakTypeTag[StrictMapOps[A, T, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.collections.sequential.StrictMapOps").toType,
          weakTypeOf[A] :: weakTypeOf[T] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictMapOps[A, T, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictMapOps[A, T, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }

  private def FamilyTag(c: Context)(these: c.Expr[Family[_]]): c.WeakTypeTag[these.value.Family] = {
    import c.{ mirror, WeakTypeTag }
    import c.universe._
    import internal._
    val TheseTpe = these.tree.symbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => these.actualType
    }
    val FamilyTpc = mirror.staticClass("basis.collections.Family").toType
    val FamilySym = FamilyTpc.member("Family": TypeName)
    val FamilyTpe = typeRef(TheseTpe, FamilySym, Nil).normalize
    WeakTypeTag[these.value.Family](FamilyTpe)
  }
}
