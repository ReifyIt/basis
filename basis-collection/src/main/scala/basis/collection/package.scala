/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object collection {
  implicit def EnumeratorOps[A](self: Enumerator[A]): EnumeratorOps[self.Self, A] =
    new EnumeratorOps[self.Self, A](self)
  
  implicit def IteratorOps[A](self: Iterator[A]): IteratorOps[A] =
    new IteratorOps[A](self)
  
  implicit def CollectionOps[A](self: Collection[A]): CollectionOps[self.Self, A] =
    new CollectionOps[self.Self, A](self)
  
  implicit def ContainerOps[A](self: Container[A]): ContainerOps[self.Self, A] =
    new ContainerOps[self.Self, A](self)
  
  implicit def SeqOps[A](self: Seq[A]): SeqOps[self.Self, A] =
    new SeqOps[self.Self, A](self)
  
  implicit def SetOps[A](self: Set[A]): SetOps[self.Self, A] =
    new SetOps[self.Self, A](self)
  
  implicit def MapOps[A, Z](self: Map[A, Z]): MapOps[self.Self, A, Z] =
    new MapOps[self.Self, A, Z](self)
  
  private[basis] final class Break extends java.lang.Throwable
  private[basis] val Break = new Break
}
