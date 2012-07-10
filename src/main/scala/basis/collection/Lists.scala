/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Lists[+A] extends Any with Iterates[A] with Listable[A] {
  import Lists._
  
  override def isEmpty: Boolean
  
  override def head: A
  
  override def tail: Listable[A]
  
  override def map[B](f: A => B): Lists[B] = new Maps[A, B](this, f)
  
  override def filter(p: A => Boolean): Lists[A] = new Filters[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Lists[B] = new Collects[A, B](this, q)
  
  override def drop(lower: Int): Lists[A] = new Drops[A](this, lower)
  
  override def take(upper: Int): Lists[A] = new Takes[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Lists[A] = drop(lower).take(upper)
  
  override def lazily: Lists[A] = this
}

object Lists {
  abstract class Abstractly[+A] extends Listable.Abstractly[A] with Lists[A]
  
  final class Projects[+A](self: Listable[A]) extends Abstractly[A] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override def tail: Listable[A] = self.tail
    override def map[B](f: A => B): Lists[B] = new Maps[A, B](self, f)
    override def filter(p: A => Boolean): Lists[A] = new Filters[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Lists[B] = new Collects[A, B](self, q)
    override def drop(lower: Int): Lists[A] = new Drops[A](self, lower)
    override def take(upper: Int): Lists[A] = new Takes[A](self, upper)
  }
  
  final class Maps[-A, +B](self: Listable[A], f: A => B) extends Abstractly[B] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: B = f(self.head)
    override lazy val tail: Listable[B] = new Maps[A, B](self.tail, f)
  }
  
  final class Filters[+A](self: Listable[A], p: A => Boolean) extends Abstractly[A] {
    private[this] var rest: Listable[A] = self
    while (!rest.isEmpty && !p(rest.head)) rest = rest.tail
    override def isEmpty: Boolean = rest.isEmpty
    override def head: A = rest.head
    override lazy val tail: Listable[A] = new Filters[A](rest.tail, p)
  }
  
  final class Collects[-A, +B](self: Listable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    private[this] var rest: Listable[A] = self
    while (!rest.isEmpty && !q.isDefinedAt(rest.head)) rest = rest.tail
    override def isEmpty: Boolean = rest.isEmpty
    override def head: B = q(rest.head)
    override lazy val tail: Listable[B] = new Collects[A, B](rest.tail, q)
  }
  
  final class Drops[+A](self: Listable[A], lower: Int) extends Abstractly[A] {
    private[this] var rest: Listable[A] = self;
    { var i = 0; while (i < lower && !rest.isEmpty) { rest = rest.tail; i += 1 } }
    override def isEmpty: Boolean = rest.isEmpty
    override def head: A = rest.head
    override def tail: Listable[A] = rest.tail
    override def take(upper: Int): Lists[A] = new Takes[A](rest, upper)
  }
  
  final class Takes[+A](self: Listable[A], upper: Int) extends Abstractly[A] {
    override def isEmpty: Boolean = upper > 0 && self.isEmpty
    override def head: A =
      if (upper > 0) self.head
      else throw new UnsupportedOperationException("empty head")
    override def tail: Listable[A] =
      if (upper > 0) new Takes[A](self.tail, upper - 1)
      else throw new UnsupportedOperationException("empty tail")
  }
}
