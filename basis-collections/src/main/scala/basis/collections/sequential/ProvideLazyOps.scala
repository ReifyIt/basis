/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Implicit conversions that add common and non-strict operations to collections. */
class ProvideLazyOps extends ProvideCommonOps {
  implicit def LazyEnumeratorOps[A](self: Enumerator[A]): LazyEnumeratorOps[A] =
    new LazyEnumeratorOps[A](self)
  
  implicit def LazyCollectionOps[A](self: Collection[A]): LazyCollectionOps[A] =
    new LazyCollectionOps[A](self)
  
  implicit def LazyIteratorOps[A](self: Iterator[A]): LazyIteratorOps[A] =
    new LazyIteratorOps[A](self)
  
  implicit def LazyContainerOps[A](self: Container[A]): LazyContainerOps[A] =
    new LazyContainerOps[A](self)
  
  implicit def LazySeqOps[A](self: Seq[A]): LazySeqOps[A] =
    new LazySeqOps[A](self)
  
  implicit def LazyLinearSeqOps[A](self: LinearSeq[A]): LazyLinearSeqOps[A] =
    new LazyLinearSeqOps[A](self)
  
  implicit def LazyIndexedSeqOps[A](self: IndexedSeq[A]): LazyIndexedSeqOps[A] =
    new LazyIndexedSeqOps[A](self)
  
  implicit def LazySetOps[A](self: Set[A]): LazySetOps[A] =
    new LazySetOps[A](self)
  
  implicit def LazyMapOps[A, T](self: Map[A, T]): LazyMapOps[A, T] =
    new LazyMapOps[A, T](self)
}
