//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

class Strict extends General {
  implicit def ArrayToStrictOps[A](xs: Array[A]): StrictArrayOps[A, Array[_]]                 = macro StrictMacros.ArrayToStrictOps[A]
  implicit def BilinearSeqToStrictOps[A](xs: BilinearSeq[A]): StrictSeqOps[A, xs.Family]      = macro StrictMacros.SeqToStrictOps[A]
  implicit def CollectionToStrictOps[A](xs: Collection[A]): StrictCollectionOps[A, xs.Family] = macro StrictMacros.CollectionToStrictOps[A]
  implicit def ContainerToStrictOps[A](xs: Container[A]): StrictContainerOps[A, xs.Family]    = macro StrictMacros.ContainerToStrictOps[A]
  implicit def IndexedSeqToStrictOps[A](xs: IndexedSeq[A]): StrictIndexedSeqOps[A, xs.Family] = macro StrictMacros.IndexedSeqToStrictOps[A]
  implicit def IteratorToStrictOps[A](xs: Iterator[A]): StrictIteratorOps[A, xs.Family]       = macro StrictMacros.IteratorToStrictOps[A]
  implicit def LinearSeqToStrictOps[A](xs: LinearSeq[A]): StrictLinearSeqOps[A, xs.Family]    = macro StrictMacros.LinearSeqToStrictOps[A]
  implicit def MapToStrictOps[A, T](xs: Map[A, T]): StrictMapOps[A, T, xs.Family]             = macro StrictMacros.MapToStrictOps[A, T]
  implicit def SeqToStringOps[A](xs: Seq[A]): StrictSeqOps[A, xs.Family]                      = macro StrictMacros.SeqToStrictOps[A]
  implicit def SetToStrictOps[A](xs: Set[A]): StrictSetOps[A, xs.Family]                      = macro StrictMacros.SetToStrictOps[A]
  implicit def TraverserToStrictOps[A](xs: Traverser[A]): StrictTraverserOps[A, xs.Family]    = macro StrictMacros.TraverserToStrictOps[A]
}

private[collections] class StrictMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, WeakTypeTag }
  import c.universe.{ Traverser => _, _ }

  def ArrayToStrictOps[A : WeakTypeTag](xs: Expr[Array[A]]): Expr[StrictArrayOps[A, Array[_]]]                         = StrictOpsFamily1[StrictArrayOps, A, Array[_]](xs)
  def CollectionToStrictOps[A : WeakTypeTag](xs: Expr[Collection[A]]): Expr[StrictCollectionOps[A, xs.value.Family]]   = StrictOps1[StrictCollectionOps, A](xs)
  def ContainerToStrictOps[A : WeakTypeTag](xs: Expr[Container[A]]): Expr[StrictContainerOps[A, xs.value.Family]]      = StrictOps1[StrictContainerOps, A](xs)
  def IndexedSeqToStrictOps[A : WeakTypeTag](xs: Expr[IndexedSeq[A]]): Expr[StrictIndexedSeqOps[A, xs.value.Family]]   = StrictOps1[StrictIndexedSeqOps, A](xs)
  def IteratorToStrictOps[A : WeakTypeTag](xs: Expr[Iterator[A]]): Expr[StrictIteratorOps[A, xs.value.Family]]         = StrictOps1[StrictIteratorOps, A](xs)
  def LinearSeqToStrictOps[A : WeakTypeTag](xs: Expr[LinearSeq[A]]): Expr[StrictLinearSeqOps[A, xs.value.Family]]      = StrictOps1[StrictLinearSeqOps, A](xs)
  def MapToStrictOps[A : WeakTypeTag, T : WeakTypeTag](xs: Expr[Map[A, T]]): Expr[StrictMapOps[A, T, xs.value.Family]] = StrictOps2[StrictMapOps, A, T](xs)
  def SeqToStrictOps[A : WeakTypeTag](xs: Expr[Seq[A]]): Expr[StrictSeqOps[A, xs.value.Family]]                        = StrictOps1[StrictSeqOps, A](xs)
  def SetToStrictOps[A : WeakTypeTag](xs: Expr[Set[A]]): Expr[StrictSetOps[A, xs.value.Family]]                        = StrictOps1[StrictSetOps, A](xs)
  def TraverserToStrictOps[A : WeakTypeTag](xs: Expr[Traverser[A]]): Expr[StrictTraverserOps[A, xs.value.Family]]      = StrictOps1[StrictTraverserOps, A](xs)

  implicit protected def StrictArrayOpsTag: WeakTypeTag[StrictArrayOps[_, _]]           = StrictOpsTag[StrictArrayOps[_, _]]("StrictArrayOps")
  implicit protected def StrictCollectionOpsTag: WeakTypeTag[StrictCollectionOps[_, _]] = StrictOpsTag[StrictCollectionOps[_, _]]("StrictCollectionOps")
  implicit protected def StrictContainerOpsTag: WeakTypeTag[StrictContainerOps[_, _]]   = StrictOpsTag[StrictContainerOps[_, _]]("StrictContainerOps")
  implicit protected def StrictIndexedSeqOpsTag: WeakTypeTag[StrictIndexedSeqOps[_, _]] = StrictOpsTag[StrictIndexedSeqOps[_, _]]("StrictIndexedSeqOps")
  implicit protected def StrictIteratorOpsTag: WeakTypeTag[StrictIteratorOps[_, _]]     = StrictOpsTag[StrictIteratorOps[_, _]]("StrictIteratorOps")
  implicit protected def StrictLinearSeqOpsTag: WeakTypeTag[StrictLinearSeqOps[_, _]]   = StrictOpsTag[StrictLinearSeqOps[_, _]]("StrictLinearSeqOps")
  implicit protected def StrictMapOpsTag: WeakTypeTag[StrictMapOps[_, _, _]]            = StrictOpsTag[StrictMapOps[_, _, _]]("StrictMapOps")
  implicit protected def StrictSeqOpsTag: WeakTypeTag[StrictSeqOps[_, _]]               = StrictOpsTag[StrictSeqOps[_, _]]("StrictSeqOps")
  implicit protected def StrictSetOpsTag: WeakTypeTag[StrictSetOps[_, _]]               = StrictOpsTag[StrictSetOps[_, _]]("StrictSetOps")
  implicit protected def StrictTraverserOpsTag: WeakTypeTag[StrictTraverserOps[_, _]]   = StrictOpsTag[StrictTraverserOps[_, _]]("StrictTraverserOps")

  implicit protected def ArrayTag: WeakTypeTag[Array[_]] = WeakTypeTag(definitions.ArrayClass.toTypeConstructor)

  protected def StrictOpsFamily1[CC[_, _], A, Family](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_, _]], A: WeakTypeTag[A], Family: WeakTypeTag[Family]): Expr[CC[A, Family]] = {
    implicit val StrictOps: WeakTypeTag[CC[A, Family]] = WeakTypeTag(appliedType(mirror.staticClass(CC.tpe.typeSymbol.fullName).toTypeConstructor, A.tpe :: Family.tpe :: Nil))
    Expr[CC[A, Family]](q"new $StrictOps($xs)")
  }

  protected def StrictOps1[CC[_, _], A](xs: Expr[Family[_]])(implicit CC: WeakTypeTag[CC[_, _]], A: WeakTypeTag[A]): Expr[CC[A, xs.value.Family]] = {
    implicit val Family: WeakTypeTag[xs.value.Family] = FamilyTag(xs)
    implicit val StrictOps = WeakTypeTag[CC[A, xs.value.Family]](appliedType(mirror.staticClass(CC.tpe.typeSymbol.fullName).toTypeConstructor, A.tpe :: Family.tpe :: Nil))
    Expr[CC[A, xs.value.Family]](q"new $StrictOps($xs)")
  }

  protected def StrictOps2[CC[_, _, _], A, T](xs: Expr[Family[_]])(implicit CC: WeakTypeTag[CC[_, _, _]], A: WeakTypeTag[A], T: WeakTypeTag[T]): Expr[CC[A, T, xs.value.Family]] = {
    implicit val Family: WeakTypeTag[xs.value.Family] = FamilyTag(xs)
    implicit val StrictOps = WeakTypeTag[CC[A, T, xs.value.Family]](appliedType(mirror.staticClass(CC.tpe.typeSymbol.fullName).toTypeConstructor, A.tpe :: T.tpe :: Family.tpe :: Nil))
    Expr[CC[A, T, xs.value.Family]](q"new $StrictOps($xs)")
  }

  protected def StrictOpsTag[CC](name: String): WeakTypeTag[CC] = WeakTypeTag(mirror.staticClass(s"basis.collections.sequential.$name").toTypeConstructor)

  protected def FamilyTag(xs: Expr[Family[_]]): WeakTypeTag[xs.value.Family] = {
    import internal._
    val TheseTpe = xs.tree.symbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => xs.actualType
    }
    val FamilyTpc = mirror.staticClass("basis.collections.Family").toType
    val FamilySym = FamilyTpc.member("Family": TypeName)
    val FamilyTpe = typeRef(TheseTpe, FamilySym, Nil).dealias
    WeakTypeTag[xs.value.Family](FamilyTpe)
  }
}
