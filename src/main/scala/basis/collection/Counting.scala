/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Counting[A] extends Any with Iterating[A] with Countable[A] {
  import Counting._
  
  override def iterator: Iterator[A]
  
  override def contains(element: A): Boolean
  
  def + (element: A): Counting[A] =
    if (this contains element) this else new Including[A](this, element)
  
  def - (element: A): Counting[A] =
    if (this contains element) new Excluding[A](this, element) else this
  
  def | (that: Countable[A]): Counting[A] = new Union[A](this, that)
  
  def & (that: Countable[A]): Counting[A] = new Intersection[A](this, that)
  
  def &~ (that: Countable[A]): Counting[A] = new Difference[A](this, that)
  
  override def lazily: Counting[A] = this
}

private[basis] object Counting {
  private[basis] abstract class Abstractly[A] extends Countable.Abstractly[A] with Counting[A]
  
  private[basis] final class Projecting[A](self: Countable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator
    override def contains(element: A): Boolean = self.contains(element)
    override def + (element: A): Counting[A] =
      if (self contains element) this else new Including[A](self, element)
    override def - (element: A): Counting[A] =
      if (self contains element) new Excluding[A](self, element) else this
    override def | (that: Countable[A]): Counting[A] = new Union[A](self, that)
    override def & (that: Countable[A]): Counting[A] = new Intersection[A](self, that)
    override def &~ (that: Countable[A]): Counting[A] = new Difference[A](self, that)
  }
  
  private[basis] final class Including[A](self: Countable[A], element: A) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator :+ element
    override def contains(element: A): Boolean = this.element == element || self.contains(element)
  }
  
  private[basis] final class Excluding[A](self: Countable[A], element: A) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator.filter(_ != element)
    override def contains(element: A): Boolean = !(this.element == element) && self.contains(element)
  }
  
  private[basis] final class Union[A](set1: Countable[A], set2: Countable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = set1.iterator ++ set2.iterator.filter(!set1.contains(_))
    override def contains(element: A): Boolean = set1.contains(element) || set2.contains(element)
  }
  
  private[basis] final class Intersection[A](set1: Countable[A], set2: Countable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = set1.iterator.filter(set2.contains(_))
    override def contains(element: A): Boolean = set1.contains(element) && set2.contains(element)
  }
  
  private[basis] final class Difference[A](set1: Countable[A], set2: Countable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = set1.iterator.filter(!set2.contains(_))
    override def contains(element: A): Boolean = set1.contains(element) && !set2.contains(element)
  }
}
