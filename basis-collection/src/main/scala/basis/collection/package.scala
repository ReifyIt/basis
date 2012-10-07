/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object collection {
  import scala.language.implicitConversions
  
  implicit def EnumeratorOps[A](self: Enumerator[A]): EnumeratorOps[A, self.Self] =
    new EnumeratorOps[A, self.Self](self)
  
  implicit def IteratorOps[A](self: Iterator[A]): IteratorOps[A] =
    new IteratorOps[A](self)
  
  implicit def CollectionOps[A](self: Collection[A]): CollectionOps[A, self.Self] =
    new CollectionOps[A, self.Self](self)
  
  implicit def ContainerOps[A](self: Container[A]): ContainerOps[A, self.Self] =
    new ContainerOps[A, self.Self](self)
  
  implicit def SeqOps[A](self: Seq[A]): SeqOps[A, self.Self] =
    new SeqOps[A, self.Self](self)
  
  implicit def ListOps[A](self: List[A]): ListOps[A, self.Self] =
    new ListOps[A, self.Self](self)
  
  implicit def ArrayOps[A](self: Array[A]): ArrayOps[A, self.Self] =
    new ArrayOps[A, self.Self](self)
  
  implicit def SetOps[A](self: Set[A]): SetOps[A, self.Self] =
    new SetOps[A, self.Self](self)
  
  implicit def MapOps[A, Z](self: Map[A, Z]): MapOps[A, Z, self.Self] =
    new MapOps[A, Z, self.Self](self)
  
  private[basis] final class Break extends java.lang.Throwable
  private[basis] val Break = new Break
}
