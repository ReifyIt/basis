/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait LinearView[+A] extends Any with IterableView[A] with Linear[A] {
  import LinearView._
  
  override def map[B](f: A => B): LinearView[B] = new Mapped[A, B](this, f)
  
  override def filter(p: A => Boolean): LinearView[A] = new Filtered[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): LinearView[B] = new Collected[A, B](this, q)
  
  override def drop(lower: Int): LinearView[A] = new Dropped[A](this, lower)
  
  override def take(upper: Int): LinearView[A] = new Taken[A](this, upper)
  
  override def slice(lower: Int, upper: Int): LinearView[A] = drop(lower).take(upper)
  
  override def view: LinearView[A] = this
}

private[basis] object LinearView {
  final class Projection[+A](self: Linear[A]) extends AbstractLinearView[A] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override def tail: Linear[A] = self.tail
    override def map[B](f: A => B): LinearView[B] = new Mapped[A, B](self, f)
    override def filter(p: A => Boolean): LinearView[A] = new Filtered[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): LinearView[B] = new Collected[A, B](self, q)
    override def drop(lower: Int): LinearView[A] = new Dropped[A](self, lower)
    override def take(upper: Int): LinearView[A] = new Taken[A](self, upper)
  }
  
  final class Mapped[-A, +B](self: Linear[A], f: A => B) extends AbstractLinearView[B] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: B = f(self.head)
    override lazy val tail: Linear[B] = new Mapped[A, B](self.tail, f)
  }
  
  final class Filtered[+A](self: Linear[A], p: A => Boolean) extends AbstractLinearView[A] {
    private[this] var rest: Linear[A] = self
    while (!rest.isEmpty && !p(rest.head)) rest = rest.tail
    override def isEmpty: Boolean = rest.isEmpty
    override def head: A = rest.head
    override lazy val tail: Linear[A] = new Filtered[A](rest.tail, p)
  }
  
  final class Collected[-A, +B](self: Linear[A], q: PartialFunction[A, B]) extends AbstractLinearView[B] {
    private[this] var rest: Linear[A] = self
    while (!rest.isEmpty && !q.isDefinedAt(rest.head)) rest = rest.tail
    override def isEmpty: Boolean = rest.isEmpty
    override def head: B = q(rest.head)
    override lazy val tail: Linear[B] = new Collected[A, B](rest.tail, q)
  }
  
  final class Dropped[+A](self: Linear[A], lower: Int) extends AbstractLinearView[A] {
    private[this] var rest: Linear[A] = self;
    { var i = 0; while (i < lower && !rest.isEmpty) { rest = rest.tail; i += 1 } }
    override def isEmpty: Boolean = rest.isEmpty
    override def head: A = rest.head
    override def tail: Linear[A] = rest.tail
    override def take(upper: Int): LinearView[A] = new Taken[A](rest, upper)
  }
  
  final class Taken[+A](self: Linear[A], upper: Int) extends AbstractLinearView[A] {
    override def isEmpty: Boolean = upper > 0 && self.isEmpty
    override def head: A =
      if (upper > 0) self.head
      else throw new UnsupportedOperationException("empty head")
    override def tail: Linear[A] =
      if (upper > 0) new Taken[A](self.tail, upper - 1)
      else throw new UnsupportedOperationException("empty tail")
  }
}

private[basis] abstract class AbstractLinearView[+A] extends AbstractLinear[A] with LinearView[A]
