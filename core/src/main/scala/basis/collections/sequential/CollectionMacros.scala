//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

private[sequential] class CollectionMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, WeakTypeTag }
  import c.universe.{ Traverser => _, _ }
  import c.universe.internal._

  implicit protected def MaybeTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Maybe[A]] =
    WeakTypeTag(appliedType(mirror.staticPackage("basis").moduleClass.typeSignature.member(TypeName("Maybe")).asType.toTypeConstructor, A.tpe :: Nil))

  implicit protected def Tuple2Tag[A, B](implicit A: WeakTypeTag[A], B: WeakTypeTag[B]): WeakTypeTag[(A, B)] =
    WeakTypeTag(appliedType(definitions.TupleClass(2).asType.toTypeConstructor, A.tpe :: B.tpe :: Nil))

  implicit protected def ArrayTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Array[A]] =
    WeakTypeTag(appliedType(definitions.ArrayClass.toTypeConstructor, A.tpe :: Nil))

  implicit protected def ArrayKindTag: WeakTypeTag[Array[_]] =
    WeakTypeTag(definitions.ArrayClass.toTypeConstructor)

  implicit protected def CollectionTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Collection[A]]            = Collection1Tag[Collection, A]("Collection")
  implicit protected def ContainerTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Container[A]]              = Collection1Tag[Container, A]("Container")
  implicit protected def IndexedSeqTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[IndexedSeq[A]]            = Collection1Tag[IndexedSeq, A]("IndexedSeq")
  implicit protected def IteratorTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Iterator[A]]                = Collection1Tag[Iterator, A]("Iterator")
  implicit protected def LinearSeqTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[LinearSeq[A]]              = Collection1Tag[LinearSeq, A]("LinearSeq")
  implicit protected def MapTag[A, T](implicit A: WeakTypeTag[A], T: WeakTypeTag[T]): WeakTypeTag[Map[A, T]] = Collection2Tag[Map, A, T]("Map")
  implicit protected def SeqTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Seq[A]]                          = Collection1Tag[Seq, A]("Seq")
  implicit protected def SetTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Set[A]]                          = Collection1Tag[Set, A]("Set")
  implicit protected def TraverserTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Traverser[A]]              = Collection1Tag[Traverser, A]("Traverser")

  implicit protected def GeneralArrayOpsTag: WeakTypeTag[GeneralArrayOps[_]]           = SequentialTag[GeneralArrayOps[_]]("GeneralArrayOps")
  implicit protected def GeneralCollectionOpsTag: WeakTypeTag[GeneralCollectionOps[_]] = SequentialTag[GeneralCollectionOps[_]]("GeneralCollectionOps")
  implicit protected def GeneralContainerOpsTag: WeakTypeTag[GeneralContainerOps[_]]   = SequentialTag[GeneralContainerOps[_]]("GeneralContainerOps")
  implicit protected def GeneralIndexedSeqOpsTag: WeakTypeTag[GeneralIndexedSeqOps[_]] = SequentialTag[GeneralIndexedSeqOps[_]]("GeneralIndexedSeqOps")
  implicit protected def GeneralIteratorOpsTag: WeakTypeTag[GeneralIteratorOps[_]]     = SequentialTag[GeneralIteratorOps[_]]("GeneralIteratorOps")
  implicit protected def GeneralLinearSeqOpsTag: WeakTypeTag[GeneralLinearSeqOps[_]]   = SequentialTag[GeneralLinearSeqOps[_]]("GeneralLinearSeqOps")
  implicit protected def GeneralMapOpsTag: WeakTypeTag[GeneralMapOps[_, _]]            = SequentialTag[GeneralMapOps[_, _]]("GeneralMapOps")
  implicit protected def GeneralSeqOpsTag: WeakTypeTag[GeneralSeqOps[_]]               = SequentialTag[GeneralSeqOps[_]]("GeneralSeqOps")
  implicit protected def GeneralSetOpsTag: WeakTypeTag[GeneralSetOps[_]]               = SequentialTag[GeneralSetOps[_]]("GeneralSetOps")
  implicit protected def GeneralTraverserOpsTag: WeakTypeTag[GeneralTraverserOps[_]]   = SequentialTag[GeneralTraverserOps[_]]("GeneralTraverserOps")

  implicit protected def NonStrictArrayOpsTag: WeakTypeTag[NonStrictArrayOps[_]]           = SequentialTag[NonStrictArrayOps[_]]("NonStrictArrayOps")
  implicit protected def NonStrictCollectionOpsTag: WeakTypeTag[NonStrictCollectionOps[_]] = SequentialTag[NonStrictCollectionOps[_]]("NonStrictCollectionOps")
  implicit protected def NonStrictContainerOpsTag: WeakTypeTag[NonStrictContainerOps[_]]   = SequentialTag[NonStrictContainerOps[_]]("NonStrictContainerOps")
  implicit protected def NonStrictIndexedSeqOpsTag: WeakTypeTag[NonStrictIndexedSeqOps[_]] = SequentialTag[NonStrictIndexedSeqOps[_]]("NonStrictIndexedSeqOps")
  implicit protected def NonStrictIteratorOpsTag: WeakTypeTag[NonStrictIteratorOps[_]]     = SequentialTag[NonStrictIteratorOps[_]]("NonStrictIteratorOps")
  implicit protected def NonStrictLinearSeqOpsTag: WeakTypeTag[NonStrictLinearSeqOps[_]]   = SequentialTag[NonStrictLinearSeqOps[_]]("NonStrictLinearSeqOps")
  implicit protected def NonStrictMapOpsTag: WeakTypeTag[NonStrictMapOps[_, _]]            = SequentialTag[NonStrictMapOps[_, _]]("NonStrictMapOps")
  implicit protected def NonStrictSeqOpsTag: WeakTypeTag[NonStrictSeqOps[_]]               = SequentialTag[NonStrictSeqOps[_]]("NonStrictSeqOps")
  implicit protected def NonStrictSetOpsTag: WeakTypeTag[NonStrictSetOps[_]]               = SequentialTag[NonStrictSetOps[_]]("NonStrictSetOps")
  implicit protected def NonStrictTraverserOpsTag: WeakTypeTag[NonStrictTraverserOps[_]]   = SequentialTag[NonStrictTraverserOps[_]]("NonStrictTraverserOps")

  implicit protected def StrictArrayOpsTag: WeakTypeTag[StrictArrayOps[_, _]]           = SequentialTag[StrictArrayOps[_, _]]("StrictArrayOps")
  implicit protected def StrictCollectionOpsTag: WeakTypeTag[StrictCollectionOps[_, _]] = SequentialTag[StrictCollectionOps[_, _]]("StrictCollectionOps")
  implicit protected def StrictContainerOpsTag: WeakTypeTag[StrictContainerOps[_, _]]   = SequentialTag[StrictContainerOps[_, _]]("StrictContainerOps")
  implicit protected def StrictIndexedSeqOpsTag: WeakTypeTag[StrictIndexedSeqOps[_, _]] = SequentialTag[StrictIndexedSeqOps[_, _]]("StrictIndexedSeqOps")
  implicit protected def StrictIteratorOpsTag: WeakTypeTag[StrictIteratorOps[_, _]]     = SequentialTag[StrictIteratorOps[_, _]]("StrictIteratorOps")
  implicit protected def StrictLinearSeqOpsTag: WeakTypeTag[StrictLinearSeqOps[_, _]]   = SequentialTag[StrictLinearSeqOps[_, _]]("StrictLinearSeqOps")
  implicit protected def StrictMapOpsTag: WeakTypeTag[StrictMapOps[_, _, _]]            = SequentialTag[StrictMapOps[_, _, _]]("StrictMapOps")
  implicit protected def StrictSeqOpsTag: WeakTypeTag[StrictSeqOps[_, _]]               = SequentialTag[StrictSeqOps[_, _]]("StrictSeqOps")
  implicit protected def StrictSetOpsTag: WeakTypeTag[StrictSetOps[_, _]]               = SequentialTag[StrictSetOps[_, _]]("StrictSetOps")
  implicit protected def StrictTraverserOpsTag: WeakTypeTag[StrictTraverserOps[_, _]]   = SequentialTag[StrictTraverserOps[_, _]]("StrictTraverserOps")

  protected def BuilderTypeTag(builder: Expr[Builder[_]]): WeakTypeTag[builder.value.type] = WeakTypeTag(builder.tree.symbol match {
    case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
    case _ => if (builder.actualType != null) builder.actualType else builder.staticType
  })

  protected def BuilderStateTag(builder: Expr[Builder[_]])(implicit BuilderTag: WeakTypeTag[builder.value.type]): WeakTypeTag[builder.value.State] =
    WeakTypeTag(typeRef(BuilderTag.tpe, mirror.staticClass("basis.collections.Builder").toType.member(TypeName("State")), Nil).dealias)

  protected def Collection1Tag[CC[_], A](name: String)(implicit A: WeakTypeTag[A]): WeakTypeTag[CC[A]] =
    WeakTypeTag(appliedType(mirror.staticClass(s"basis.collections.$name").toTypeConstructor, A.tpe :: Nil))

  protected def Collection2Tag[CC[_, _], A, T](name: String)(implicit A: WeakTypeTag[A], T: WeakTypeTag[T]): WeakTypeTag[Map[A, T]] =
    WeakTypeTag(appliedType(mirror.staticClass(s"basis.collections.$name").toTypeConstructor, A.tpe :: T.tpe :: Nil))

  protected def SequentialTag[CC](name: String): WeakTypeTag[CC] =
    WeakTypeTag(mirror.staticClass(s"basis.collections.sequential.$name").toTypeConstructor)

  protected def FamilyTag(xs: Expr[Family[_]]): WeakTypeTag[xs.value.Family] =
    WeakTypeTag(typeRef(StableType(xs), mirror.staticClass("basis.Family").toType.member(TypeName("Family")), Nil).dealias)

  protected def StableType(expr: Expr[_]): Type = expr.tree.symbol match {
    case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
    case _ => if (expr.actualType != null) expr.actualType else expr.staticType
  }

  protected def GeneralOps1[CC[_], A](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    implicit val GeneralOps = WeakTypeTag[CC[A]](appliedType(CC.tpe.typeConstructor, A.tpe :: Nil))
    Expr[CC[A]](q"new $GeneralOps($xs)")
  }

  protected def GeneralOps2[CC[_, _], A, T](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_, _]], A: WeakTypeTag[A], T: WeakTypeTag[T]): Expr[CC[A, T]] = {
    implicit val GeneralOps = WeakTypeTag[CC[A, T]](appliedType(CC.tpe.typeConstructor, A.tpe :: T.tpe :: Nil))
    Expr[CC[A, T]](q"new $GeneralOps($xs)")
  }

  protected def NonStrictOps1[CC[_], A](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    implicit val NonStrictOps = WeakTypeTag[CC[A]](appliedType(CC.tpe.typeConstructor, A.tpe :: Nil))
    Expr[CC[A]](q"new $NonStrictOps($xs)")
  }

  protected def NonStrictOps2[CC[_, _], A, T](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_, _]], A: WeakTypeTag[A], T: WeakTypeTag[T]): Expr[CC[A, T]] = {
    implicit val NonStrictOps = WeakTypeTag[CC[A, T]](appliedType(CC.tpe.typeConstructor, A.tpe :: T.tpe :: Nil))
    Expr[CC[A, T]](q"new $NonStrictOps($xs)")
  }

  protected def StrictOpsFamily1[CC[_, _], A, Family](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_, _]], A: WeakTypeTag[A], Family: WeakTypeTag[Family]): Expr[CC[A, Family]] = {
    implicit val StrictOps: WeakTypeTag[CC[A, Family]] = WeakTypeTag(appliedType(CC.tpe.typeConstructor, A.tpe :: Family.tpe :: Nil))
    Expr[CC[A, Family]](q"new $StrictOps($xs)")
  }

  protected def StrictOps1[CC[_, _], A](xs: Expr[Family[_]])(implicit CC: WeakTypeTag[CC[_, _]], A: WeakTypeTag[A]): Expr[CC[A, xs.value.Family]] = {
    implicit val Family: WeakTypeTag[xs.value.Family] = FamilyTag(xs)
    implicit val StrictOps = WeakTypeTag[CC[A, xs.value.Family]](appliedType(CC.tpe.typeConstructor, A.tpe :: Family.tpe :: Nil))
    Expr[CC[A, xs.value.Family]](q"new $StrictOps($xs)")
  }

  protected def StrictOps2[CC[_, _, _], A, T](xs: Expr[Family[_]])(implicit CC: WeakTypeTag[CC[_, _, _]], A: WeakTypeTag[A], T: WeakTypeTag[T]): Expr[CC[A, T, xs.value.Family]] = {
    implicit val Family: WeakTypeTag[xs.value.Family] = FamilyTag(xs)
    implicit val StrictOps = WeakTypeTag[CC[A, T, xs.value.Family]](appliedType(CC.tpe.typeConstructor, A.tpe :: T.tpe :: Family.tpe :: Nil))
    Expr[CC[A, T, xs.value.Family]](q"new $StrictOps($xs)")
  }
}
