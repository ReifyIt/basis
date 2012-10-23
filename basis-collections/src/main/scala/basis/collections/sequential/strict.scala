/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Implicit conversions to extend collections with strictly evaluated operations. */
object strict {
  implicit def BasicEnumeratorOps[A](self: Enumerator[A]): BasicEnumeratorOps[A] =
    new BasicEnumeratorOps[A](self)
  
  implicit def BasicCollectionOps[A](self: Collection[A]): BasicCollectionOps[A] =
    new BasicCollectionOps[A](self)
  
  implicit def BasicIteratorOps[A](self: Iterator[A]): BasicIteratorOps[A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def BasicContainerOps[A](self: Container[A]): BasicContainerOps[A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def BasicSeqOps[A](self: Seq[A]): BasicSeqOps[A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def BasicSetOps[A](self: Set[A]): BasicSetOps[A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def BasicMapOps[A, T](self: Map[A, T]): BasicMapOps[A, T] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def EagerEnumeratorOps[A](self: Enumerator[A]): EagerEnumeratorOps[self.Self, A] =
    new EagerEnumeratorOps[self.Self, A](self)
  
  implicit def EagerCollectionOps[A](self: Collection[A]): EagerCollectionOps[self.Self, A] =
    new EagerCollectionOps[self.Self, A](self)
  
  implicit def EagerIteratorOps[A](self: Iterator[A]): EagerIteratorOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def EagerContainerOps[A](self: Container[A]): EagerContainerOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def EagerSeqOps[A](self: Seq[A]): EagerSeqOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def EagerSetOps[A](self: Set[A]): EagerSetOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def EagerMapOps[A, T](self: Map[A, T]): EagerMapOps[self.Self, A, T] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}
