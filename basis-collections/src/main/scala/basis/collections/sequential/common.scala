/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Implicit conversions that add common operations to collections. */
class common {
  implicit def CommonEnumeratorOps[A](self: Enumerator[A]): CommonEnumeratorOps[A, self.Family] =
    new CommonEnumeratorOps[A, self.Family](self)
  
  implicit def CommonCollectionOps[A](self: Collection[A]): CommonCollectionOps[A, self.Family] =
    new CommonCollectionOps[A, self.Family](self)
  
  implicit def CommonIteratorOps[A](self: Iterator[A]): CommonIteratorOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonContainerOps[A](self: Container[A]): CommonContainerOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonSeqOps[A](self: Seq[A]): CommonSeqOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonLinearSeqOps[A](self: LinearSeq[A]): CommonLinearSeqOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonIndexedSeqOps[A](self: IndexedSeq[A]): CommonIndexedSeqOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonSetOps[A](self: Set[A]): CommonSetOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CommonMapOps[A, T](self: Map[A, T]): CommonMapOps[A, T, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}

/** Implicit conversions that add common operations to collections. */
object common extends common
