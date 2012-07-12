/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Listed[+A] extends Any with Sequenced[A] with Listable[A] {
  import Listed._
  
  override def isEmpty: Boolean
  
  override def head: A
  
  override def tail: Listable[A]
  
  override def map[B](f: A => B): Listed[B] = new Mapped[A, B](this, f)
  
  override def filter(p: A => Boolean): Listed[A] = new Filtered[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Listed[B] = new Collected[A, B](this, q)
  
  override def drop(lower: Int): Listed[A] = new Dropped[A](this, lower)
  
  override def take(upper: Int): Listed[A] = new Taken[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Listed[A] = drop(lower).take(upper)
  
  override def lazily: Listed[A] = this
}

object Listed {
  abstract class Abstractly[+A] extends Listable.Abstractly[A] with Listed[A]
  
  final class Projected[+A](self: Listable[A]) extends Abstractly[A] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override def tail: Listable[A] = self.tail
    override def map[B](f: A => B): Listed[B] = new Mapped[A, B](self, f)
    override def filter(p: A => Boolean): Listed[A] = new Filtered[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Listed[B] = new Collected[A, B](self, q)
    override def drop(lower: Int): Listed[A] = new Dropped[A](self, lower)
    override def take(upper: Int): Listed[A] = new Taken[A](self, upper)
  }
  
  final class Mapped[-A, +B](self: Listable[A], f: A => B) extends Abstractly[B] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: B = f(self.head)
    override lazy val tail: Listable[B] = new Mapped[A, B](self.tail, f)
  }
  
  final class Filtered[+A](private[this] var self: Listable[A], p: A => Boolean) extends Abstractly[A] {
    while (!self.isEmpty && !p(self.head)) self = self.tail
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override lazy val tail: Listable[A] = new Filtered[A](self.tail, p)
  }
  
  final class Collected[-A, +B](private[this] var self: Listable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    while (!self.isEmpty && !q.isDefinedAt(self.head)) self = self.tail
    override def isEmpty: Boolean = self.isEmpty
    override def head: B = q(self.head)
    override lazy val tail: Listable[B] = new Collected[A, B](self.tail, q)
  }
  
  final class Dropped[+A](private[this] var self: Listable[A], lower: Int) extends Abstractly[A] {
    { var i = 0; while (i < lower && !self.isEmpty) { self = self.tail; i += 1 } }
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override def tail: Listable[A] = self.tail
    override def take(upper: Int): Listed[A] = new Dropped[A](self, upper)
  }
  
  final class Taken[+A](self: Listable[A], upper: Int) extends Abstractly[A] {
    override def isEmpty: Boolean = upper > 0 && self.isEmpty
    override def head: A =
      if (upper > 0) self.head
      else throw new UnsupportedOperationException("empty head")
    override def tail: Listable[A] =
      if (upper > 0) new Taken[A](self.tail, upper - 1)
      else throw new UnsupportedOperationException("empty tail")
  }
}
