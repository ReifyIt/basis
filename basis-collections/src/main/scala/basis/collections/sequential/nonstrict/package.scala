/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import traversable._

package object nonstrict {
  @inline implicit def EnumeratorOps[A](self: Enumerator[A]): EnumeratorView[A, self.Parent] =
    new EnumeratorView[A, self.Parent](self)
  
  @inline implicit def IteratorOps[A](self: Iterator[A]): IteratorView[A, self.Parent] =
    new IteratorView[A, self.Parent](self)
  
  @inline implicit def CollectionOps[A](self: Collection[A]): CollectionView[A, self.Parent] =
    new CollectionView[A, self.Parent](self)
  
  @inline implicit def ContainerOps[A](self: Container[A]): ContainerView[A, self.Parent] =
    new ContainerView[A, self.Parent](self)
  
  @inline implicit def SeqOps[A](self: Seq[A]): SeqView[A, self.Parent] =
    new SeqView[A, self.Parent](self)
  
  @inline implicit def LinearSeqOps[A](self: LinearSeq[A]): LinearSeqView[A, self.Parent] =
    new LinearSeqView[A, self.Parent](self)
  
  @inline implicit def IndexedSeqOps[A](self: IndexedSeq[A]): IndexedSeqView[A, self.Parent] =
    new IndexedSeqView[A, self.Parent](self)
  
  @inline implicit def SetOps[A](self: Set[A]): SetView[A, self.Parent] =
    new SetView[A, self.Parent](self)
  
  @inline implicit def MapOps[A, T](self: Map[A, T]): MapView[A, T, self.Parent] =
    new MapView[A, T, self.Parent](self)
}
