//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.reflect.macros.Context

class General {
  implicit def ArrayToGeneralOps[A](xs: Array[A]): GeneralArrayOps[A]                = macro General.ArrayToGeneralOps[A]
  implicit def BilinearSeqToGeneralOps[A](xs: BilinearSeq[A]): GeneralSeqOps[A]      = macro General.SeqToGeneralOps[A]
  implicit def CollectionToGeneralOps[A](xs: Collection[A]): GeneralCollectionOps[A] = macro General.CollectionToGeneralOps[A]
  implicit def ContainerToGeneralOps[A](xs: Container[A]): GeneralContainerOps[A]    = macro General.ContainerToGeneralOps[A]
  implicit def IndexedSeqToGeneralOps[A](xs: IndexedSeq[A]): GeneralIndexedSeqOps[A] = macro General.IndexedSeqToGeneralOps[A]
  implicit def IteratorToGeneralOps[A](xs: Iterator[A]): GeneralIteratorOps[A]       = macro General.IteratorToGeneralOps[A]
  implicit def LinearSeqToGeneralOps[A](xs: LinearSeq[A]): GeneralLinearSeqOps[A]    = macro General.LinearSeqToGeneralOps[A]
  implicit def MapToGeneralOps[A, T](xs: Map[A, T]): GeneralMapOps[A, T]             = macro General.MapToGeneralOps[A, T]
  implicit def SeqToGeneralOps[A](xs: Seq[A]): GeneralSeqOps[A]                      = macro General.SeqToGeneralOps[A]
  implicit def SetToGeneralOps[A](xs: Set[A]): GeneralSetOps[A]                      = macro General.SetToGeneralOps[A]
  implicit def TraverserToGeneralOps[A](xs: Traverser[A]): GeneralTraverserOps[A]    = macro General.TraverserToGeneralOps[A]
}
private[collections] object General {
  private def wrapGeneral[Arg: c.WeakTypeTag, Wrapper: c.WeakTypeTag](c: Context)(xs: c.Expr[Arg]): c.Expr[Wrapper] = {
    import c.universe._
    implicit val WrapperType = weakTypeOf[Wrapper]
    c.Expr(q"new $WrapperType($xs)")
  }
  def ArrayToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Array[A]])                  = wrapGeneral[Array[A], GeneralArrayOps[A]](c)(xs)
  def CollectionToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Collection[A]])        = wrapGeneral[Collection[A], GeneralCollectionOps[A]](c)(xs)
  def ContainerToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Container[A]])          = wrapGeneral[Container[A], GeneralContainerOps[A]](c)(xs)
  def IndexedSeqToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[IndexedSeq[A]])        = wrapGeneral[IndexedSeq[A], GeneralIndexedSeqOps[A]](c)(xs)
  def IteratorToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Iterator[A]])            = wrapGeneral[Iterator[A], GeneralIteratorOps[A]](c)(xs)
  def LinearSeqToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[LinearSeq[A]])          = wrapGeneral[LinearSeq[A], GeneralLinearSeqOps[A]](c)(xs)
  def MapToGeneralOps[A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(xs: c.Expr[Map[A, T]]) = wrapGeneral[Map[A, T], GeneralMapOps[A, T]](c)(xs)
  def SeqToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Seq[A]])                      = wrapGeneral[Seq[A], GeneralSeqOps[A]](c)(xs)
  def SetToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Set[A]])                      = wrapGeneral[Set[A], GeneralSetOps[A]](c)(xs)
  def TraverserToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Traverser[A]])          = wrapGeneral[Traverser[A], GeneralTraverserOps[A]](c)(xs)
}
