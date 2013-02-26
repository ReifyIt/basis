/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Implicit conversions that add general and strictly-evaluated operations to collections.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Strict
  * 
  * @groupname  General   General collection extensions
  * @groupprio  General   1
  * 
  * @groupname  Strict    Strict collection extensions
  * @groupprio  Strict    2
  */
class Strict extends General {
  /** Implicitly provides strictly evaluated operations for arrays.
    * @group Strict */
  implicit def StrictArrayOps[A](these: Array[A]): StrictArrayOps[A, Array[_]] =
    macro Strict.StrictArrayOps[A]
  
  /** Implicitly provides strictly evaluated operations for enumerators.
    * @group Strict */
  implicit def StrictEnumeratorOps[A](these: Enumerator[A]): StrictEnumeratorOps[A, these.Family] =
    macro Strict.StrictEnumeratorOps[A]
  
  /** Implicitly provides strictly evaluated operations for iterators.
    * @group Strict */
  implicit def StrictIteratorOps[A](these: Iterator[A]): StrictIteratorOps[A, these.Family] =
    macro Strict.StrictIteratorOps[A]
  
  /** Implicitly provides strictly evaluated operations for collections.
    * @group Strict */
  implicit def StrictCollectionOps[A](these: Collection[A]): StrictCollectionOps[A, these.Family] =
    macro Strict.StrictCollectionOps[A]
  
  /** Implicitly provides strictly evaluated operations for containers.
    * @group Strict */
  implicit def StrictContainerOps[A](these: Container[A]): StrictContainerOps[A, these.Family] =
    macro Strict.StrictContainerOps[A]
  
  /** Implicitly provides strictly evaluated operations for sequences.
    * @group Strict */
  implicit def StrictSeqOps[A](these: Seq[A]): StrictSeqOps[A, these.Family] =
    macro Strict.StrictSeqOps[A]
  
  /** Implicitly provides strictly evaluated operations for indexes.
    * @group Strict */
  implicit def StrictIndexOps[A](these: Index[A]): StrictIndexOps[A, these.Family] =
    macro Strict.StrictIndexOps[A]
  
  /** Implicitly provides strictly evaluated operations for sides.
    * @group Strict */
  implicit def StrictSideOps[A](these: Side[A]): StrictSideOps[A, these.Family] =
    macro Strict.StrictSideOps[A]
  
  /** Implicitly provides strictly evaluated operations for sets.
    * @group Strict */
  implicit def StrictSetOps[A](these: Set[A]): StrictSetOps[A, these.Family] =
    macro Strict.StrictSetOps[A]
  
  /** Implicitly provides strictly evaluated operations for maps.
    * @group Strict */
  implicit def StrictMapOps[A, T](these: Map[A, T]): StrictMapOps[A, T, these.Family] =
    macro Strict.StrictMapOps[A, T]
}

private[sequential] object Strict {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def StrictArrayOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Array[A]])
    : c.Expr[StrictArrayOps[A, Array[_]]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val ArrayTpe = appliedType(mirror.staticClass("scala.Array").toType, WildcardType :: Nil)
    implicit val StrictArrayOpsATag =
      WeakTypeTag[StrictArrayOps[A, Array[_]]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictArrayOps").toType,
          weakTypeOf[A] :: appliedType(mirror.staticClass("scala.Array").toType, WildcardType :: Nil) :: Nil))
    Expr[StrictArrayOps[A, Array[_]]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictArrayOps[A, Array[_]]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def StrictEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[StrictEnumeratorOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val StrictEnumeratorOpsATag =
      WeakTypeTag[StrictEnumeratorOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictEnumeratorOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictEnumeratorOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictEnumeratorOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def StrictIteratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[StrictIteratorOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val StrictIteratorOpsATag =
      WeakTypeTag[StrictIteratorOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictIteratorOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictIteratorOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictIteratorOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def StrictCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[StrictCollectionOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val StrictCollectionOpsATag =
      WeakTypeTag[StrictCollectionOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictCollectionOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictCollectionOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictCollectionOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def StrictContainerOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[StrictContainerOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val StrictContainerOpsATag =
      WeakTypeTag[StrictContainerOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictContainerOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictContainerOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictContainerOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def StrictSeqOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[StrictSeqOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val StrictSeqOpsATag =
      WeakTypeTag[StrictSeqOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictSeqOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictSeqOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictSeqOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def StrictIndexOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Index[A]])
    : c.Expr[StrictIndexOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val StrictIndexOpsATag =
      WeakTypeTag[StrictIndexOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictIndexOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictIndexOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictIndexOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def StrictSideOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Side[A]])
    : c.Expr[StrictSideOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val StrictSideOpsATag =
      WeakTypeTag[StrictSideOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictSideOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictSideOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictSideOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def StrictSetOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[StrictSetOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val StrictSetOpsATag =
      WeakTypeTag[StrictSetOps[A, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictSetOps").toType,
          weakTypeOf[A] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictSetOps[A, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictSetOps[A, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  def StrictMapOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[StrictMapOps[A, T, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    implicit val StrictMapOpsATTag =
      WeakTypeTag[StrictMapOps[A, T, these.value.Family]](
        appliedType(
          mirror.staticClass("basis.sequential.StrictMapOps").toType,
          weakTypeOf[A] :: weakTypeOf[T] :: FamilyTag(c)(these).tpe :: Nil))
    Expr[StrictMapOps[A, T, these.value.Family]](
      Apply(
        Select(New(TypeTree(weakTypeOf[StrictMapOps[A, T, these.value.Family]])), nme.CONSTRUCTOR),
        these.tree :: Nil))
  }
  
  private def FamilyTag(c: Context)(these: c.Expr[Family[_]]): c.WeakTypeTag[these.value.Family] = {
    import c.{mirror, WeakTypeTag}
    import c.universe._
    val TheseTpe = these.tree.symbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => these.actualType
    }
    val FamilyTpc = mirror.staticClass("basis.collections.Family").toType
    val FamilySym = FamilyTpc member newTypeName("Family")
    val FamilyTpe = typeRef(TheseTpe, FamilySym, Nil).normalize
    WeakTypeTag[these.value.Family](FamilyTpe)
  }
}
