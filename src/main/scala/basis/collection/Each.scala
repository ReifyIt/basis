/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Each[+A] extends Any {
  type Self
  
  def foreach[U](f: A => U): Unit
}

object Each {
  import scala.runtime.AbstractFunction1
  import scala.language.implicitConversions
  
  @inline implicit def ForEach[A](self: Each[A]): ForEach[self.Self, A] =
    new ForEach[self.Self, A](self)
  
  final class ForEach[From, A](val __ : Each[A]) extends AnyVal {
    import __.foreach
    
    def select[B](q: PartialFunction[A, B]): Option[B] = {
      val f = new Select(q)
      try foreach(f) catch { case e: Break => () }
      f.result
    }
    
    def fold[B >: A](z: B)(op: (B, B) => B): B = {
      val f = new FoldLeft(z)(op)
      foreach(f)
      f.result
    }
    
    def reduce[B >: A](op: (B, B) => B): B = {
      val f = new ReduceLeft(op)
      foreach(f)
      if (f.isDefined) f.result else throw new UnsupportedOperationException
    }
    
    def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
      val f = new ReduceLeft(op)
      foreach(f)
      if (f.isDefined) Some(f.result) else None
    }
    
    def find(p: A => Boolean): Option[A] = {
      val f = new Find(p)
      try foreach(f) catch { case e: Break => () }
      f.result
    }
    
    def forall(p: A => Boolean): Boolean = {
      val f = new Forall(p)
      try foreach(f) catch { case e: Break => () }
      f.result
    }
    
    def exists(p: A => Boolean): Boolean = {
      val f = new Exists(p)
      try foreach(f) catch { case e: Break => () }
      f.result
    }
    
    def count(p: A => Boolean): Int = {
      val f = new Count(p)
      foreach(f)
      f.result
    }
    
    def map[B](f: A => B)(implicit make: Make[From, B]): make.What = {
      foreach(new AppendMap(f, make))
      make.result
    }
    
    def flatMap[B](f: A => Each[B])(implicit make: Make[From, B]): make.What = {
      foreach(new AppendFlatMap(f, make))
      make.result
    }
    
    def filter(p: A => Boolean)(implicit make: Make[From, A]): make.What = {
      foreach(new AppendFilter(p, make))
      make.result
    }
    
    def withFilter(p: A => Boolean): Each[A] = new WithFilter(__, p)
    
    def collect[B](q: PartialFunction[A, B])(implicit make: Make[From, B]): make.What = {
      foreach(new AppendCollect(q, make))
      make.result
    }
  }
  
  private[basis] final class WithFilter[+A](self: Each[A], p: A => Boolean) extends Each[A] {
    override def foreach[U](f: A => U): Unit = self.foreach(new Filter(f, p))
  }
  
  private[basis] final class Select[-A, +B](q: PartialFunction[A, B]) extends AbstractFunction1[A, Unit] {
    private[this] var a: Option[B] = None
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) { a = Some(q(x)); throw Break }
    def result: Option[B] = a
  }
  
  private[basis] final class FoldLeft[-A, +B](z: B)(op: (B, A) => B) extends AbstractFunction1[A, Unit] {
    private[this] var a: B = z
    override def apply(x: A): Unit = a = op(a, x)
    def result: B = a
  }
  
  private[basis] final class ReduceLeft[-A, +B >: A](op: (B, A) => B) extends AbstractFunction1[A, Unit] {
    private[this] var b: Boolean = false
    private[this] var a: B = _
    override def apply(x: A): Unit = if (!b) { a = x; b = true } else a = op(a, x)
    def isDefined: Boolean = b
    def result: B = a
  }
  
  private[basis] final class Find[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var a: Option[A] = None
    override def apply(x: A): Unit = if (p(x)) { a = Some(x); throw Break }
    def result: Option[A] = a
  }
  
  private[basis] final class Forall[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var b: Boolean = true
    override def apply(x: A): Unit = if (!p(x)) { b = false; throw Break }
    def result: Boolean = b
  }
  
  private[basis] final class Exists[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var b: Boolean = false
    override def apply(x: A): Unit = if (p(x)) { b = true; throw Break }
    def result: Boolean = b
  }
  
  private[basis] final class Count[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var total: Int = 0
    override def apply(x: A): Unit = if (p(x)) total += 1
    def result: Int = total
  }
  
  private[basis] final class AddString[A](s: java.lang.StringBuilder, sep: String) extends AbstractFunction1[A, Unit] {
    private[this] var q = true
    override def apply(x: A): Unit = (if (q) { q = false; s } else s.append(sep)).append(x)
  }
  
  private[basis] final class FlatMap[-A, +B, +U](f: B => U, g: A => Each[B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = g(x).foreach(f)
  }
  
  private[basis] final class Filter[-A, +U](f: A => U, p: A => Boolean) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) f(x)
  }
  
  private[basis] final class Collect[-A, +B, +U](f: B => U, q: PartialFunction[A, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) f(q(x))
  }
  
  private[basis] final class DropWhile[-A, +U](f: A => U, p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A) {
      if (!taking && !p(x)) taking = true
      if (taking) f(x)
    }
  }
  
  private[basis] final class TakeWhile[-A, +U](f: A => U, p: A => Boolean) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) f(x) else throw Break
  }
  
  private[basis] final class Drop[-A, +U](f: A => U, lower: Int) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i >= lower) f(x) else i += 1
  }
  
  private[basis] final class Take[-A, +U](f: A => U, upper: Int) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < upper) { f(x); i += 1 } else throw Break
  }
  
  private[basis] final class Slice[-A, +U](f: A => U, lower: Int, upper: Int) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < upper) { if (i >= lower) f(x); i += 1 } else throw Break
  }
  
  private[basis] final class Append[-A](make: Make[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = make += x
  }
  
  private[basis] final class AppendMap[-A, +B](f: A => B, make: Make[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = make += f(x)
  }
  
  private[basis] final class AppendFlatMap[-A, +B](f: A => Each[B], make: Make[_, B]) extends AbstractFunction1[A, Unit] {
    private[this] val append = new Append(make)
    override def apply(x: A): Unit = f(x).foreach(append)
  }
  
  private[basis] final class AppendFilter[-A](p: A => Boolean, make: Make[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) make += x
  }
  
  private[basis] final class AppendCollect[-A, B](q: PartialFunction[A, B], make: Make[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) make += q(x)
  }
}
