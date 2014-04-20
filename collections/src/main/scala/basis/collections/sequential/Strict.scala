//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

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

private[collections] class StrictMacros(override val c: blackbox.Context) extends CollectionMacros(c) {
  import c.{ Expr, WeakTypeTag }
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
}
