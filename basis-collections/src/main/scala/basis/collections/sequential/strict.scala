/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.collections.general._

/** Implicit conversions that add common and strict operations to collections. */
class strict extends common {
  implicit def StrictEnumeratorOps[A](self: Enumerator[A]): StrictEnumeratorOps[A, self.Parent] =
    new StrictEnumeratorOps[A, self.Parent](self)
  
  implicit def StrictCollectionOps[A](self: Collection[A]): StrictCollectionOps[A, self.Parent] =
    new StrictCollectionOps[A, self.Parent](self)
  
  implicit def StrictIteratorOps[A](self: Iterator[A]): StrictIteratorOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictContainerOps[A](self: Container[A]): StrictContainerOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictSeqOps[A](self: Seq[A]): StrictSeqOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictLinearSeqOps[A](self: LinearSeq[A]): StrictLinearSeqOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictIndexedSeqOps[A](self: IndexedSeq[A]): StrictIndexedSeqOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictSetOps[A](self: Set[A]): StrictSetOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictMapOps[A, T](self: Map[A, T]): StrictMapOps[A, T, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}

/** Implicit conversions that add common and strict operations to collections. */
object strict extends strict
