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
  private def new1[Construct: c.WeakTypeTag, Arg: c.WeakTypeTag](c: Context)(xs: c.Expr[Arg]): c.Expr[Construct] = {
    import c.universe._
    implicit val ConstructType = weakTypeOf[Construct]
    c.Expr(q"new $ConstructType($xs)")
  }

  def ArrayToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Array[A]])                  = new1[GeneralArrayOps[A], Array[A]](c)(xs)
  def CollectionToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Collection[A]])        = new1[GeneralCollectionOps[A], Collection[A]](c)(xs)
  def ContainerToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Container[A]])          = new1[GeneralContainerOps[A], Container[A]](c)(xs)
  def IndexedSeqToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[IndexedSeq[A]])        = new1[GeneralIndexedSeqOps[A], IndexedSeq[A]](c)(xs)
  def IteratorToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Iterator[A]])            = new1[GeneralIteratorOps[A], Iterator[A]](c)(xs)
  def LinearSeqToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[LinearSeq[A]])          = new1[GeneralLinearSeqOps[A], LinearSeq[A]](c)(xs)
  def MapToGeneralOps[A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(xs: c.Expr[Map[A, T]]) = new1[GeneralMapOps[A, T], Map[A, T]](c)(xs)
  def SeqToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Seq[A]])                      = new1[GeneralSeqOps[A], Seq[A]](c)(xs)
  def SetToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Set[A]])                      = new1[GeneralSetOps[A], Set[A]](c)(xs)
  def TraverserToGeneralOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Traverser[A]])          = new1[GeneralTraverserOps[A], Traverser[A]](c)(xs)
}
