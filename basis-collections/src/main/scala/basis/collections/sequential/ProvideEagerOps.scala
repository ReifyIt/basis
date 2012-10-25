/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Implicit conversions that add common and strict operations to collections. */
class ProvideEagerOps extends ProvideCommonOps {
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
  
  implicit def EagerLinearSeqOps[A](self: LinearSeq[A]): EagerLinearSeqOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def EagerSetOps[A](self: Set[A]): EagerSetOps[self.Self, A] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def EagerMapOps[A, T](self: Map[A, T]): EagerMapOps[self.Self, A, T] =
    throw new java.lang.UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}
