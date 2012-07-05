/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait LinearView[+A] extends Any with IterableView[A] with Linear[A] { self =>
  override def map[B](f: A => B): LinearView[B] = new Mapped[B](f)
  
  override def filter(p: A => Boolean): LinearView[A] = new Filtered(p)
  
  override def collect[B](q: PartialFunction[A, B]): LinearView[B] = new Collected[B](q)
  
  override def drop(lower: Int): LinearView[A] = new Dropped(lower)
  
  override def take(upper: Int): LinearView[A] = new Taken(upper)
  
  override def slice(lower: Int, upper: Int): LinearView[A] = new Sliced(lower, upper)
  
  override def view: LinearView[A] = this
  
  private final class Mapped[+B](f: A => B) extends AbstractLinearView[B] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: B = f(self.head)
    override def tail: Linear[B] = self.tail.view map f
  }
  
  private final class Filtered(p: A => Boolean) extends AbstractLinearView[A] {
    private[this] var rest: Linear[A] = _
    
    private[this] def element: Linear[A] = synchronized {
      if (rest == null) {
        rest = self
        while (!rest.isEmpty && !p(rest.head)) rest = rest.tail
      }
      rest
    }
    
    override def isEmpty: Boolean = element.isEmpty
    override def head: A = element.head
    override def tail: Linear[A] = element.tail.view filter p
  }
  
  private final class Collected[+B](q: PartialFunction[A, B]) extends AbstractLinearView[B] {
    private[this] var rest: Linear[A] = _
    
    private[this] def element: Linear[A] = synchronized {
      if (rest == null) {
        rest = self
        while (!rest.isEmpty && !q.isDefinedAt(rest.head)) rest = rest.tail
      }
      rest
    }
    
    override def isEmpty: Boolean = element.isEmpty
    override def head: B = q(element.head)
    override def tail: Linear[B] = element.tail.view collect q
  }
  
  private final class Dropped(lower: Int) extends AbstractLinearView[A] {
    private[this] var rest: Linear[A] = _
    
    private[this] def element: Linear[A] = synchronized {
      if (rest == null) {
        var i = 0
        rest = self
        while (i < lower && !rest.isEmpty) { rest = rest.tail; i += 1 }
      }
      rest
    }
    
    override def isEmpty: Boolean = element.isEmpty
    override def head: A = element.head
    override def tail: Linear[A] = element.tail
  }
  
  private final class Taken(upper: Int) extends AbstractLinearView[A] {
    override def isEmpty: Boolean = 0 < upper && self.isEmpty
    
    override def head: A =
      if (0 < upper) self.head
      else throw new UnsupportedOperationException("empty head")
    
    override def tail: Linear[A] =
      if (0 < upper) self.tail.view take (upper - 1)
      else throw new UnsupportedOperationException("empty tail")
  }
  
  private final class Sliced(from: Int, until: Int) extends AbstractLinearView[A] {
    private[this] var lower: Int = math.max(0, from)
    private[this] var upper: Int = math.max(lower, until)
    private[this] var rest: Linear[A] = _
    
    private[this] def element: Linear[A] = synchronized {
      if (lower < upper && rest == null) {
        var i = 0
        rest = self
        while (i < lower && !rest.isEmpty) { rest = rest.tail; i += 1 }
      }
      rest
    }
    
    override def isEmpty: Boolean = lower < upper && element.isEmpty
    
    override def head: A =
      if (lower < upper) element.head
      else throw new UnsupportedOperationException("empty head")
    
    override def tail: Linear[A] =
      if (lower < upper) element.tail.view take (upper - lower - 1)
      else throw new UnsupportedOperationException("empty tail")
  }
}

private[basis] abstract class AbstractLinearView[+A] extends AbstractLinear[A] with LinearView[A]
