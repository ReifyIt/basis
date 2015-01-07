//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.reflect.macros._

class General extends GeneralPriority2 {
  implicit def ArrayToGeneralOps[A](xs: Array[A]): GeneralArrayOps[A]                = macro GeneralMacros.ArrayToGeneralOps[A]
  implicit def BilinearSeqToGeneralOps[A](xs: BilinearSeq[A]): GeneralSeqOps[A]      = macro GeneralMacros.SeqToGeneralOps[A]
}

private[collections] trait GeneralPriority2 extends GeneralPriority3 {
  implicit def IndexedSeqToGeneralOps[A](xs: IndexedSeq[A]): GeneralIndexedSeqOps[A] = macro GeneralMacros.IndexedSeqToGeneralOps[A]
  implicit def LinearSeqToGeneralOps[A](xs: LinearSeq[A]): GeneralLinearSeqOps[A]    = macro GeneralMacros.LinearSeqToGeneralOps[A]
}

private[collections] trait GeneralPriority3 {
  implicit def CollectionToGeneralOps[A](xs: Collection[A]): GeneralCollectionOps[A] = macro GeneralMacros.CollectionToGeneralOps[A]
  implicit def ContainerToGeneralOps[A](xs: Container[A]): GeneralContainerOps[A]    = macro GeneralMacros.ContainerToGeneralOps[A]
  implicit def IteratorToGeneralOps[A](xs: Iterator[A]): GeneralIteratorOps[A]       = macro GeneralMacros.IteratorToGeneralOps[A]
  implicit def MapToGeneralOps[A, T](xs: Map[A, T]): GeneralMapOps[A, T]             = macro GeneralMacros.MapToGeneralOps[A, T]
  implicit def SeqToGeneralOps[A](xs: Seq[A]): GeneralSeqOps[A]                      = macro GeneralMacros.SeqToGeneralOps[A]
  implicit def SetToGeneralOps[A](xs: Set[A]): GeneralSetOps[A]                      = macro GeneralMacros.SetToGeneralOps[A]
  implicit def TraverserToGeneralOps[A](xs: Traverser[A]): GeneralTraverserOps[A]    = macro GeneralMacros.TraverserToGeneralOps[A]
}

private[collections] class GeneralMacros(override val c: blackbox.Context) extends CollectionMacros(c) {
  import c.{ Expr, WeakTypeTag }
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
}
