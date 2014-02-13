//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

class NonStrict extends General {
  implicit def ArrayToNonStrictOps[A](xs: Array[A]): NonStrictArrayOps[A]                = macro NonStrict.ArrayToNonStrictOps[A]
  implicit def TraverserToNonStrictOps[A](xs: Traverser[A]): NonStrictTraverserOps[A]    = macro NonStrict.TraverserToNonStrictOps[A]
  implicit def IteratorToNonStrictOps[A](xs: Iterator[A]): NonStrictIteratorOps[A]       = macro NonStrict.IteratorToNonStrictOps[A]
  implicit def CollectionToNonStrictOps[A](xs: Collection[A]): NonStrictCollectionOps[A] = macro NonStrict.CollectionToNonStrictOps[A]
  implicit def ContainerToNonStrictOps[A](xs: Container[A]): NonStrictContainerOps[A]    = macro NonStrict.ContainerToNonStrictOps[A]
  implicit def SeqToNonStrictOps[A](xs: Seq[A]): NonStrictSeqOps[A]                      = macro NonStrict.SeqToNonStrictOps[A]
  implicit def IndexedSeqToNonStrictOps[A](xs: IndexedSeq[A]): NonStrictIndexedSeqOps[A] = macro NonStrict.IndexedSeqToNonStrictOps[A]
  implicit def LinearSeqToNonStrictOps[A](xs: LinearSeq[A]): NonStrictLinearSeqOps[A]    = macro NonStrict.LinearSeqToNonStrictOps[A]
  implicit def BilinearSeqToNonStrictOps[A](xs: BilinearSeq[A]): NonStrictSeqOps[A]      = macro NonStrict.SeqToNonStrictOps[A]
  implicit def SetToNonStrictOps[A](xs: Set[A]): NonStrictSetOps[A]                      = macro NonStrict.SetToNonStrictOps[A]
  implicit def MapToNonStrictOps[A, T](xs: Map[A, T]): NonStrictMapOps[A, T]             = macro NonStrict.MapToNonStrictOps[A, T]
}

private[collections] object NonStrict {
  import scala.reflect.macros.Context

  def ArrayToNonStrictOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Array[A]])                  = General.new1[NonStrictArrayOps[A], Array[A]](c)(xs)
  def TraverserToNonStrictOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Traverser[A]])          = General.new1[NonStrictTraverserOps[A], Traverser[A]](c)(xs)
  def IteratorToNonStrictOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Iterator[A]])            = General.new1[NonStrictIteratorOps[A], Iterator[A]](c)(xs)
  def CollectionToNonStrictOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Collection[A]])        = General.new1[NonStrictCollectionOps[A], Collection[A]](c)(xs)
  def ContainerToNonStrictOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Container[A]])          = General.new1[NonStrictContainerOps[A], Container[A]](c)(xs)
  def SeqToNonStrictOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Seq[A]])                      = General.new1[NonStrictSeqOps[A], Seq[A]](c)(xs)
  def IndexedSeqToNonStrictOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[IndexedSeq[A]])        = General.new1[NonStrictIndexedSeqOps[A], IndexedSeq[A]](c)(xs)
  def LinearSeqToNonStrictOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[LinearSeq[A]])          = General.new1[NonStrictLinearSeqOps[A], LinearSeq[A]](c)(xs)
  def SetToNonStrictOps[A: c.WeakTypeTag](c: Context)(xs: c.Expr[Set[A]])                      = General.new1[NonStrictSetOps[A], Set[A]](c)(xs)
  def MapToNonStrictOps[A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(xs: c.Expr[Map[A, T]]) = General.new1[NonStrictMapOps[A, T], Map[A, T]](c)(xs)
}
