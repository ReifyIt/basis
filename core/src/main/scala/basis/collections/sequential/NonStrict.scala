//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.reflect.macros._

class NonStrict extends General with NonStrictPriority2 {
  implicit def ArrayToNonStrictOps[A](xs: Array[A]): NonStrictArrayOps[A]                = macro NonStrictMacros.ArrayToNonStrictOps[A]
  implicit def BilinearSeqToNonStrictOps[A](xs: BilinearSeq[A]): NonStrictSeqOps[A]      = macro NonStrictMacros.SeqToNonStrictOps[A]
}

private[collections] trait NonStrictPriority2 extends NonStrictPriority3 {
  implicit def IndexedSeqToNonStrictOps[A](xs: IndexedSeq[A]): NonStrictIndexedSeqOps[A] = macro NonStrictMacros.IndexedSeqToNonStrictOps[A]
  implicit def LinearSeqToNonStrictOps[A](xs: LinearSeq[A]): NonStrictLinearSeqOps[A]    = macro NonStrictMacros.LinearSeqToNonStrictOps[A]
}

private[collections] trait NonStrictPriority3 {
  implicit def CollectionToNonStrictOps[A](xs: Collection[A]): NonStrictCollectionOps[A] = macro NonStrictMacros.CollectionToNonStrictOps[A]
  implicit def ContainerToNonStrictOps[A](xs: Container[A]): NonStrictContainerOps[A]    = macro NonStrictMacros.ContainerToNonStrictOps[A]
  implicit def IteratorToNonStrictOps[A](xs: Iterator[A]): NonStrictIteratorOps[A]       = macro NonStrictMacros.IteratorToNonStrictOps[A]
  implicit def MapToNonStrictOps[A, T](xs: Map[A, T]): NonStrictMapOps[A, T]             = macro NonStrictMacros.MapToNonStrictOps[A, T]
  implicit def SeqToNonStrictOps[A](xs: Seq[A]): NonStrictSeqOps[A]                      = macro NonStrictMacros.SeqToNonStrictOps[A]
  implicit def SetToNonStrictOps[A](xs: Set[A]): NonStrictSetOps[A]                      = macro NonStrictMacros.SetToNonStrictOps[A]
  implicit def TraverserToNonStrictOps[A](xs: Traverser[A]): NonStrictTraverserOps[A]    = macro NonStrictMacros.TraverserToNonStrictOps[A]
}

private[collections] class NonStrictMacros(override val c: blackbox.Context) extends CollectionMacros(c) {
  import c.{ Expr, WeakTypeTag }
  import c.universe.{ Traverser => _, _ }

  def ArrayToNonStrictOps[A : WeakTypeTag](xs: Expr[Array[A]]): Expr[NonStrictArrayOps[A]]                  = NonStrictOps1[NonStrictArrayOps, A](xs)
  def CollectionToNonStrictOps[A : WeakTypeTag](xs: Expr[Collection[A]]): Expr[NonStrictCollectionOps[A]]   = NonStrictOps1[NonStrictCollectionOps, A](xs)
  def ContainerToNonStrictOps[A : WeakTypeTag](xs: Expr[Container[A]]): Expr[NonStrictContainerOps[A]]      = NonStrictOps1[NonStrictContainerOps, A](xs)
  def IndexedSeqToNonStrictOps[A : WeakTypeTag](xs: Expr[IndexedSeq[A]]): Expr[NonStrictIndexedSeqOps[A]]   = NonStrictOps1[NonStrictIndexedSeqOps, A](xs)
  def IteratorToNonStrictOps[A : WeakTypeTag](xs: Expr[Iterator[A]]): Expr[NonStrictIteratorOps[A]]         = NonStrictOps1[NonStrictIteratorOps, A](xs)
  def LinearSeqToNonStrictOps[A : WeakTypeTag](xs: Expr[LinearSeq[A]]): Expr[NonStrictLinearSeqOps[A]]      = NonStrictOps1[NonStrictLinearSeqOps, A](xs)
  def MapToNonStrictOps[A : WeakTypeTag, T : WeakTypeTag](xs: Expr[Map[A, T]]): Expr[NonStrictMapOps[A, T]] = NonStrictOps2[NonStrictMapOps, A, T](xs)
  def SeqToNonStrictOps[A : WeakTypeTag](xs: Expr[Seq[A]]): Expr[NonStrictSeqOps[A]]                        = NonStrictOps1[NonStrictSeqOps, A](xs)
  def SetToNonStrictOps[A : WeakTypeTag](xs: Expr[Set[A]]): Expr[NonStrictSetOps[A]]                        = NonStrictOps1[NonStrictSetOps, A](xs)
  def TraverserToNonStrictOps[A : WeakTypeTag](xs: Expr[Traverser[A]]): Expr[NonStrictTraverserOps[A]]      = NonStrictOps1[NonStrictTraverserOps, A](xs)
}
