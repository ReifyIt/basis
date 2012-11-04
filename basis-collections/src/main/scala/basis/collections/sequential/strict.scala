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
  implicit def StrictEnumeratorOps[A](self: Enumerator[A]): StrictEnumeratorOps[A, self.Family] =
    new StrictEnumeratorOps[A, self.Family](self)
  
  implicit def StrictCollectionOps[A](self: Collection[A]): StrictCollectionOps[A, self.Family] =
    new StrictCollectionOps[A, self.Family](self)
  
  implicit def StrictIteratorOps[A](self: Iterator[A]): StrictIteratorOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictContainerOps[A](self: Container[A]): StrictContainerOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictSeqOps[A](self: Seq[A]): StrictSeqOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictLinearSeqOps[A](self: LinearSeq[A]): StrictLinearSeqOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictIndexedSeqOps[A](self: IndexedSeq[A]): StrictIndexedSeqOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictSetOps[A](self: Set[A]): StrictSetOps[A, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def StrictMapOps[A, T](self: Map[A, T]): StrictMapOps[A, T, self.Family] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}

/** Implicit conversions that add common and strict operations to collections. */
object strict extends strict
