//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.reflect.macros._

class NonStrict extends General {
  implicit def ArrayToNonStrictOps[A](xs: Array[A]): NonStrictArrayOps[A]                = macro NonStrictMacros.ArrayToNonStrictOps[A]
  implicit def BilinearSeqToNonStrictOps[A](xs: BilinearSeq[A]): NonStrictSeqOps[A]      = macro NonStrictMacros.SeqToNonStrictOps[A]
  implicit def CollectionToNonStrictOps[A](xs: Collection[A]): NonStrictCollectionOps[A] = macro NonStrictMacros.CollectionToNonStrictOps[A]
  implicit def ContainerToNonStrictOps[A](xs: Container[A]): NonStrictContainerOps[A]    = macro NonStrictMacros.ContainerToNonStrictOps[A]
  implicit def IndexedSeqToNonStrictOps[A](xs: IndexedSeq[A]): NonStrictIndexedSeqOps[A] = macro NonStrictMacros.IndexedSeqToNonStrictOps[A]
  implicit def IteratorToNonStrictOps[A](xs: Iterator[A]): NonStrictIteratorOps[A]       = macro NonStrictMacros.IteratorToNonStrictOps[A]
  implicit def LinearSeqToNonStrictOps[A](xs: LinearSeq[A]): NonStrictLinearSeqOps[A]    = macro NonStrictMacros.LinearSeqToNonStrictOps[A]
  implicit def MapToNonStrictOps[A, T](xs: Map[A, T]): NonStrictMapOps[A, T]             = macro NonStrictMacros.MapToNonStrictOps[A, T]
  implicit def SeqToNonStrictOps[A](xs: Seq[A]): NonStrictSeqOps[A]                      = macro NonStrictMacros.SeqToNonStrictOps[A]
  implicit def SetToNonStrictOps[A](xs: Set[A]): NonStrictSetOps[A]                      = macro NonStrictMacros.SetToNonStrictOps[A]
  implicit def TraverserToNonStrictOps[A](xs: Traverser[A]): NonStrictTraverserOps[A]    = macro NonStrictMacros.TraverserToNonStrictOps[A]
}

private[collections] class NonStrictMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, WeakTypeTag }
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

  implicit protected def NonStrictArrayOpsTag: WeakTypeTag[NonStrictArrayOps[_]]           = NonStrictOpsTag[NonStrictArrayOps[_]]("NonStrictArrayOps")
  implicit protected def NonStrictCollectionOpsTag: WeakTypeTag[NonStrictCollectionOps[_]] = NonStrictOpsTag[NonStrictCollectionOps[_]]("NonStrictCollectionOps")
  implicit protected def NonStrictContainerOpsTag: WeakTypeTag[NonStrictContainerOps[_]]   = NonStrictOpsTag[NonStrictContainerOps[_]]("NonStrictContainerOps")
  implicit protected def NonStrictIndexedSeqOpsTag: WeakTypeTag[NonStrictIndexedSeqOps[_]] = NonStrictOpsTag[NonStrictIndexedSeqOps[_]]("NonStrictIndexedSeqOps")
  implicit protected def NonStrictIteratorOpsTag: WeakTypeTag[NonStrictIteratorOps[_]]     = NonStrictOpsTag[NonStrictIteratorOps[_]]("NonStrictIteratorOps")
  implicit protected def NonStrictLinearSeqOpsTag: WeakTypeTag[NonStrictLinearSeqOps[_]]   = NonStrictOpsTag[NonStrictLinearSeqOps[_]]("NonStrictLinearSeqOps")
  implicit protected def NonStrictMapOpsTag: WeakTypeTag[NonStrictMapOps[_, _]]            = NonStrictOpsTag[NonStrictMapOps[_, _]]("NonStrictMapOps")
  implicit protected def NonStrictSeqOpsTag: WeakTypeTag[NonStrictSeqOps[_]]               = NonStrictOpsTag[NonStrictSeqOps[_]]("NonStrictSeqOps")
  implicit protected def NonStrictSetOpsTag: WeakTypeTag[NonStrictSetOps[_]]               = NonStrictOpsTag[NonStrictSetOps[_]]("NonStrictSetOps")
  implicit protected def NonStrictTraverserOpsTag: WeakTypeTag[NonStrictTraverserOps[_]]   = NonStrictOpsTag[NonStrictTraverserOps[_]]("NonStrictTraverserOps")

  protected def NonStrictOps1[CC[_], A](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    implicit val NonStrictOps = WeakTypeTag[CC[A]](appliedType(mirror.staticClass(CC.tpe.typeSymbol.fullName).toTypeConstructor, A.tpe :: Nil))
    Expr[CC[A]](q"new $NonStrictOps($xs)")
  }

  protected def NonStrictOps2[CC[_, _], A, T](xs: Expr[_])(implicit CC: WeakTypeTag[CC[_, _]], A: WeakTypeTag[A], T: WeakTypeTag[T]): Expr[CC[A, T]] = {
    implicit val NonStrictOps = WeakTypeTag[CC[A, T]](appliedType(mirror.staticClass(CC.tpe.typeSymbol.fullName).toTypeConstructor, A.tpe :: T.tpe :: Nil))
    Expr[CC[A, T]](q"new $NonStrictOps($xs)")
  }

  protected def NonStrictOpsTag[CC](name: String): WeakTypeTag[CC] = WeakTypeTag(mirror.staticClass(s"basis.collections.sequential.$name").toTypeConstructor)
}
