/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.collections.traversable._

/** Implicit conversions that add general operations to collections. */
class general {
  implicit def GeneralEnumeratorOps[A](self: Enumerator[A]): GeneralEnumeratorOps[A, self.Parent] =
    new GeneralEnumeratorOps[A, self.Parent](self)
  
  implicit def GeneralCollectionOps[A](self: Collection[A]): GeneralCollectionOps[A, self.Parent] =
    new GeneralCollectionOps[A, self.Parent](self)
  
  implicit def GeneralIteratorOps[A](self: Iterator[A]): GeneralIteratorOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def GeneralContainerOps[A](self: Container[A]): GeneralContainerOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def GeneralSeqOps[A](self: Seq[A]): GeneralSeqOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def GeneralLinearSeqOps[A](self: LinearSeq[A]): GeneralLinearSeqOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def GeneralIndexedSeqOps[A](self: IndexedSeq[A]): GeneralIndexedSeqOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def GeneralSetOps[A](self: Set[A]): GeneralSetOps[A, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def GeneralMapOps[A, T](self: Map[A, T]): GeneralMapOps[A, T, self.Parent] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}

/** Implicit conversions that add general operations to collections. */
object general extends general
