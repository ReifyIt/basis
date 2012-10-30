/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Implicit conversions that add common and non-strict operations to collections. */
class nonstrict extends common {
  implicit def NonStrictEnumeratorOps[A](self: Enumerator[A]): NonStrictEnumeratorOps[A] =
    new NonStrictEnumeratorOps[A](self)
  
  implicit def NonStrictCollectionOps[A](self: Collection[A]): NonStrictCollectionOps[A] =
    new NonStrictCollectionOps[A](self)
  
  implicit def NonStrictIteratorOps[A](self: Iterator[A]): NonStrictIteratorOps[A] =
    new NonStrictIteratorOps[A](self)
  
  implicit def NonStrictContainerOps[A](self: Container[A]): NonStrictContainerOps[A] =
    new NonStrictContainerOps[A](self)
  
  implicit def NonStrictSeqOps[A](self: Seq[A]): NonStrictSeqOps[A] =
    new NonStrictSeqOps[A](self)
  
  implicit def NonStrictLinearSeqOps[A](self: LinearSeq[A]): NonStrictLinearSeqOps[A] =
    new NonStrictLinearSeqOps[A](self)
  
  implicit def NonStrictIndexedSeqOps[A](self: IndexedSeq[A]): NonStrictIndexedSeqOps[A] =
    new NonStrictIndexedSeqOps[A](self)
  
  implicit def NonStrictSetOps[A](self: Set[A]): NonStrictSetOps[A] =
    new NonStrictSetOps[A](self)
  
  implicit def NonStrictMapOps[A, T](self: Map[A, T]): NonStrictMapOps[A, T] =
    new NonStrictMapOps[A, T](self)
}

/** Implicit conversions that add command and non-strict operations to collections. */
object nonstrict extends nonstrict
