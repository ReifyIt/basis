/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Implicit conversions that add common and strict operations to collections. */
class strict extends common {
  implicit def StrictEnumeratorOps[A](self: Enumerator[A]): StrictEnumeratorOps[self.Self, A] =
    new StrictEnumeratorOps[self.Self, A](self)
  
  implicit def StrictCollectionOps[A](self: Collection[A]): StrictCollectionOps[self.Self, A] =
    new StrictCollectionOps[self.Self, A](self)
  
  implicit def StrictIteratorOps[A](self: Iterator[A]): StrictIteratorOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictContainerOps[A](self: Container[A]): StrictContainerOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictSeqOps[A](self: Seq[A]): StrictSeqOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictLinearSeqOps[A](self: LinearSeq[A]): StrictLinearSeqOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictIndexedSeqOps[A](self: IndexedSeq[A]): StrictIndexedSeqOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictSetOps[A](self: Set[A]): StrictSetOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictMapOps[A, T](self: Map[A, T]): StrictMapOps[self.Self, A, T] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}

/** Implicit conversions that add common and strict operations to collections. */
object strict extends strict
