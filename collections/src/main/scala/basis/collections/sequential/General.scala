//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.reflect.macros._

class General {
  implicit def ArrayToGeneralOps[A](xs: Array[A]): GeneralArrayOps[A]                = macro GeneralMacros.ArrayToGeneralOps[A]
  implicit def BilinearSeqToGeneralOps[A](xs: BilinearSeq[A]): GeneralSeqOps[A]      = macro GeneralMacros.SeqToGeneralOps[A]
  implicit def CollectionToGeneralOps[A](xs: Collection[A]): GeneralCollectionOps[A] = macro GeneralMacros.CollectionToGeneralOps[A]
  implicit def ContainerToGeneralOps[A](xs: Container[A]): GeneralContainerOps[A]    = macro GeneralMacros.ContainerToGeneralOps[A]
  implicit def IndexedSeqToGeneralOps[A](xs: IndexedSeq[A]): GeneralIndexedSeqOps[A] = macro GeneralMacros.IndexedSeqToGeneralOps[A]
  implicit def IteratorToGeneralOps[A](xs: Iterator[A]): GeneralIteratorOps[A]       = macro GeneralMacros.IteratorToGeneralOps[A]
  implicit def LinearSeqToGeneralOps[A](xs: LinearSeq[A]): GeneralLinearSeqOps[A]    = macro GeneralMacros.LinearSeqToGeneralOps[A]
  implicit def MapToGeneralOps[A, T](xs: Map[A, T]): GeneralMapOps[A, T]             = macro GeneralMacros.MapToGeneralOps[A, T]
  implicit def SeqToGeneralOps[A](xs: Seq[A]): GeneralSeqOps[A]                      = macro GeneralMacros.SeqToGeneralOps[A]
  implicit def SetToGeneralOps[A](xs: Set[A]): GeneralSetOps[A]                      = macro GeneralMacros.SetToGeneralOps[A]
  implicit def TraverserToGeneralOps[A](xs: Traverser[A]): GeneralTraverserOps[A]    = macro GeneralMacros.TraverserToGeneralOps[A]
}

private[collections] class GeneralMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, WeakTypeTag }
  import c.universe.{ Traverser => _, _ }

  def ArrayToGeneralOps[A : WeakTypeTag](xs: Expr[Array[A]]): Expr[GeneralArrayOps[A]]                  = GeneralOps1[GeneralArrayOps, A](xs)
  def CollectionToGeneralOps[A : WeakTypeTag](xs: Expr[Collection[A]]): Expr[GeneralCollectionOps[A]]   = GeneralOps1[GeneralCollectionOps, A](xs)
  def ContainerToGeneralOps[A : WeakTypeTag](xs: Expr[Container[A]]): Expr[GeneralContainerOps[A]]      = GeneralOps1[GeneralContainerOps, A](xs)
  def IndexedSeqToGeneralOps[A : WeakTypeTag](xs: Expr[IndexedSeq[A]]): Expr[GeneralIndexedSeqOps[A]]   = GeneralOps1[GeneralIndexedSeqOps, A](xs)
  def IteratorToGeneralOps[A : WeakTypeTag](xs: Expr[Iterator[A]]): Expr[GeneralIteratorOps[A]]         = GeneralOps1[GeneralIteratorOps, A](xs)
  def LinearSeqToGeneralOps[A : WeakTypeTag](xs: Expr[LinearSeq[A]]): Expr[GeneralLinearSeqOps[A]]      = GeneralOps1[GeneralLinearSeqOps, A](xs)
  def MapToGeneralOps[A : WeakTypeTag, T : WeakTypeTag](xs: Expr[Map[A, T]]): Expr[GeneralMapOps[A, T]] = GeneralOps2[GeneralMapOps, A, T](xs)
  def SeqToGeneralOps[A : WeakTypeTag](xs: Expr[Seq[A]]): Expr[GeneralSeqOps[A]]                        = GeneralOps1[GeneralSeqOps, A](xs)
  def SetToGeneralOps[A : WeakTypeTag](xs: Expr[Set[A]]): Expr[GeneralSetOps[A]]                        = GeneralOps1[GeneralSetOps, A](xs)
  def TraverserToGeneralOps[A : WeakTypeTag](xs: Expr[Traverser[A]]): Expr[GeneralTraverserOps[A]]      = GeneralOps1[GeneralTraverserOps, A](xs)

  implicit protected def GeneralArrayOpsTag: WeakTypeTag[GeneralArrayOps[_]]           = GeneralOpsTag[GeneralArrayOps[_]]("GeneralArrayOps")
  implicit protected def GeneralCollectionOpsTag: WeakTypeTag[GeneralCollectionOps[_]] = GeneralOpsTag[GeneralCollectionOps[_]]("GeneralCollectionOps")
  implicit protected def GeneralContainerOpsTag: WeakTypeTag[GeneralContainerOps[_]]   = GeneralOpsTag[GeneralContainerOps[_]]("GeneralContainerOps")
  implicit protected def GeneralIndexedSeqOpsTag: WeakTypeTag[GeneralIndexedSeqOps[_]] = GeneralOpsTag[GeneralIndexedSeqOps[_]]("GeneralIndexedSeqOps")
  implicit protected def GeneralIteratorOpsTag: WeakTypeTag[GeneralIteratorOps[_]]     = GeneralOpsTag[GeneralIteratorOps[_]]("GeneralIteratorOps")
  implicit protected def GeneralLinearSeqOpsTag: WeakTypeTag[GeneralLinearSeqOps[_]]   = GeneralOpsTag[GeneralLinearSeqOps[_]]("GeneralLinearSeqOps")
  implicit protected def GeneralMapOpsTag: WeakTypeTag[GeneralMapOps[_, _]]            = GeneralOpsTag[GeneralMapOps[_, _]]("GeneralMapOps")
  implicit protected def GeneralSeqOpsTag: WeakTypeTag[GeneralSeqOps[_]]               = GeneralOpsTag[GeneralSeqOps[_]]("GeneralSeqOps")
  implicit protected def GeneralSetOpsTag: WeakTypeTag[GeneralSetOps[_]]               = GeneralOpsTag[GeneralSetOps[_]]("GeneralSetOps")
  implicit protected def GeneralTraverserOpsTag: WeakTypeTag[GeneralTraverserOps[_]]   = GeneralOpsTag[GeneralTraverserOps[_]]("GeneralTraverserOps")

  protected def GeneralOps1[CC[_], A](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    implicit val GeneralOps = WeakTypeTag[CC[A]](appliedType(mirror.staticClass(CC.tpe.typeSymbol.fullName).toTypeConstructor, A.tpe :: Nil))
    Expr[CC[A]](q"new $GeneralOps($xs)")
  }

  protected def GeneralOps2[CC[_, _], A, T](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_, _]], A: WeakTypeTag[A], T: WeakTypeTag[T]): Expr[CC[A, T]] = {
    implicit val GeneralOps = WeakTypeTag[CC[A, T]](appliedType(mirror.staticClass(CC.tpe.typeSymbol.fullName).toTypeConstructor, A.tpe :: T.tpe :: Nil))
    Expr[CC[A, T]](q"new $GeneralOps($xs)")
  }

  protected def GeneralOpsTag[CC](name: String): WeakTypeTag[CC] = WeakTypeTag(mirror.staticClass(s"basis.collections.sequential.$name").toTypeConstructor)
}

private[collections] object General {
  def typed[Required: c.WeakTypeTag](c: Context)(expr: c.universe.Tree): c.Expr[Required] = {
    import c.universe._
    implicit val RequiredType = weakTypeOf[Required]
    c.Expr(q"$expr: $RequiredType")
  }

  def new1[Construct: c.WeakTypeTag, Arg: c.WeakTypeTag](c: Context)(xs: c.Expr[Arg]): c.Expr[Construct] = {
    import c.universe._
    implicit val ConstructType = weakTypeOf[Construct]
    c.Expr(q"new $ConstructType($xs)")
  }
}
