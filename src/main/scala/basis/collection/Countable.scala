/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Countable[A] extends Any with Iterable[A] {
  override def iterator: Iterator[A]
  
  def contains(element: A): Boolean
  
  override def eagerly: Counted[A] = new Counted.Projected[A](this)
  
  override def lazily: Counting[A] = new Counting.Projecting[A](this)
}

private[basis] object Countable {
  private[basis] abstract class Abstractly[A] extends Iterable.Abstractly[A] with Countable[A]
}
