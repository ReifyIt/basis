/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

final class SeqOps[+A, +Self](self: Seq[A]) {
  def withFilter(p: A => Boolean): Seq[A] =
    new SeqWithFilter(self, p)
}

private[basis] final class SeqWithFilter[+A](self: Seq[A], p: A => Boolean) extends Seq[A] {
  override def iterator: Iterator[A] = self.iterator filter p
  
  protected override def foreach[U](f: A => U): Unit =
    traverse(self)(new Traversers.Filter(p, f))
}
