/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

final class CollectionOps[+A, +Self](self: Collection[A]) {
  def withFilter(p: A => Boolean): Collection[A] =
    new CollectionWithFilter(self, p)
  
  def dropWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(self)(new Traversers.DropWhileInto(p, buffer))
    buffer.check
  }
  
  def takeWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(self)(new Traversers.TakeWhileInto(p, buffer))
    buffer.check
  }
  
  def span(p: A => Boolean)(implicit bufferA: Buffer[Self, A], bufferB: Buffer[Self, A])
    : (bufferA.State, bufferB.State) = {
    traverse(self)(new Traversers.SpanInto(p, bufferA, bufferB))
    (bufferA.check, bufferB.check)
  }
  
  def drop(lower: Int)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(self)(new Traversers.DropInto(lower, buffer))
    buffer.check
  }
  
  def take(upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(self)(new Traversers.TakeInto(upper, buffer))
    buffer.check
  }
  
  def slice(lower: Int, upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(self)(new Traversers.SliceInto(lower, upper, buffer))
    buffer.check
  }
  
  def ++ [B >: A](that: Collection[B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    val f = new Traversers.AddInto(buffer)
    traverse(self)(f)
    traverse(that)(f)
    buffer.check
  }
}

private[basis] final class CollectionWithFilter[+A](self: Collection[A], p: A => Boolean) extends Collection[A] {
  protected override def foreach[U](f: A => U): Unit =
    traverse(self)(new Traversers.Filter(p, f))
}