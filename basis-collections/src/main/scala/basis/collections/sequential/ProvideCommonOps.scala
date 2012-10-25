/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Implicit conversions that add common operations to collections. */
class ProvideCommonOps {
  implicit def CommonEnumeratorOps[A](self: Enumerator[A]): CommonEnumeratorOps[self.Self, A] =
    new CommonEnumeratorOps[self.Self, A](self)
  
  implicit def CommonCollectionOps[A](self: Collection[A]): CommonCollectionOps[self.Self, A] =
    new CommonCollectionOps[self.Self, A](self)
  
  implicit def CommonIteratorOps[A](self: Iterator[A]): CommonIteratorOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonContainerOps[A](self: Container[A]): CommonContainerOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonSeqOps[A](self: Seq[A]): CommonSeqOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonLinearSeqOps[A](self: LinearSeq[A]): CommonLinearSeqOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonSetOps[A](self: Set[A]): CommonSetOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonMapOps[A, T](self: Map[A, T]): CommonMapOps[self.Self, A, T] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}
