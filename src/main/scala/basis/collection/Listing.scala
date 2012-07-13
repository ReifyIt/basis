/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Listing[+A] extends Any with Sequencing[A] with Listable[A] {
  import Listing._
  
  override def isEmpty: Boolean
  
  override def head: A
  
  override def tail: Listable[A]
  
  override def map[B](f: A => B): Listing[B] = new Mapping[A, B](this, f)
  
  override def filter(p: A => Boolean): Listing[A] = new Filtering[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Listing[B] = new Collecting[A, B](this, q)
  
  override def drop(lower: Int): Listing[A] = new Dropping[A](this, lower)
  
  override def take(upper: Int): Listing[A] = new Taking[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Listing[A] = drop(lower).take(upper)
  
  override def lazily: Listing[A] = this
}

private[basis] object Listing {
  private[basis] abstract class Abstractly[+A] extends Listable.Abstractly[A] with Listing[A]
  
  private[basis] final class Projecting[+A](self: Listable[A]) extends Abstractly[A] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override def tail: Listable[A] = self.tail
    override def map[B](f: A => B): Listing[B] = new Mapping[A, B](self, f)
    override def filter(p: A => Boolean): Listing[A] = new Filtering[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Listing[B] = new Collecting[A, B](self, q)
    override def drop(lower: Int): Listing[A] = new Dropping[A](self, lower)
    override def take(upper: Int): Listing[A] = new Taking[A](self, upper)
  }
  
  private[basis] final class Mapping[-A, +B](self: Listable[A], f: A => B) extends Abstractly[B] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: B = f(self.head)
    override lazy val tail: Listable[B] = new Mapping[A, B](self.tail, f)
  }
  
  private[basis] final class Filtering[+A](private[this] var self: Listable[A], p: A => Boolean) extends Abstractly[A] {
    while (!self.isEmpty && !p(self.head)) self = self.tail
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override lazy val tail: Listable[A] = new Filtering[A](self.tail, p)
  }
  
  private[basis] final class Collecting[-A, +B](private[this] var self: Listable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    while (!self.isEmpty && !q.isDefinedAt(self.head)) self = self.tail
    override def isEmpty: Boolean = self.isEmpty
    override def head: B = q(self.head)
    override lazy val tail: Listable[B] = new Collecting[A, B](self.tail, q)
  }
  
  private[basis] final class Dropping[+A](private[this] var self: Listable[A], lower: Int) extends Abstractly[A] {
    { var i = 0; while (i < lower && !self.isEmpty) { self = self.tail; i += 1 } }
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override def tail: Listable[A] = self.tail
    override def take(upper: Int): Listing[A] = new Taking[A](self, upper)
  }
  
  private[basis] final class Taking[+A](self: Listable[A], upper: Int) extends Abstractly[A] {
    override def isEmpty: Boolean = upper > 0 && self.isEmpty
    override def head: A =
      if (upper > 0) self.head
      else throw new UnsupportedOperationException("empty head")
    override def tail: Listable[A] =
      if (upper > 0) new Taking[A](self.tail, upper - 1)
      else throw new UnsupportedOperationException("empty tail")
  }
}
