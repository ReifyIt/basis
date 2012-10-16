/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

private[basis] object Iterators {
  import scala.annotation.tailrec
  
  final class Collect[-A, +B](self: Iterator[A], q: scala.PartialFunction[A, B]) extends Iterator[B] {
    @tailrec override def isEmpty: Boolean =
      self.isEmpty || !q.isDefinedAt(self.head) || { self.step(); isEmpty }
    
    @tailrec override def head: B = {
      val x = self.head
      if (q.isDefinedAt(x)) q(x)
      else { self.step(); head }
    }
    
    override def step(): Unit = self.step()
    
    override def dup: Iterator[B] = new Collect[A, B](self.dup, q)
  }
  
  final class Map[-A, +B](self: Iterator[A], f: A => B) extends Iterator[B] {
    override def isEmpty: Boolean = self.isEmpty
    
    override def head: B = f(self.head)
    
    override def step(): Unit = self.step()
    
    override def dup: Iterator[B] = new Map[A, B](self.dup, f)
  }
  
  final class FlatMap[-A, +B] private
      (outer: Iterator[A], f: A => Iterator[B], private[this] var inner: Iterator[B])
    extends Iterator[B] {
    
    def this(outer: Iterator[A], f: A => Iterator[B]) = this(outer, f, Iterator.empty)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (outer.isEmpty || { inner = f(outer.head); outer.step(); isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!outer.isEmpty) { inner = f(outer.head); outer.step(); head }
      else Iterator.empty.head
    }
    
    @tailrec override def step() {
      if (!inner.isEmpty) inner.step()
      else if (!outer.isEmpty) { inner = f(outer.head); outer.step(); step() }
      else Iterator.empty.step()
    }
    
    override def dup: Iterator[B] = new FlatMap[A, B](outer.dup, f, inner.dup)
  }
  
  final class FlatMapContainer[-A, +B]
      (outer: Iterator[A], f: A => Container[B], private[this] var inner: Iterator[B])
    extends Iterator[B] {
    
    def this(outer: Iterator[A], f: A => Container[B]) = this(outer, f, Iterator.empty)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (outer.isEmpty || { inner = f(outer.head).iterator; outer.step(); isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!outer.isEmpty) { inner = f(outer.head).iterator; outer.step(); head }
      else Iterator.empty.head
    }
    
    @tailrec override def step() {
      if (!inner.isEmpty) inner.step()
      else if (!outer.isEmpty) { inner = f(outer.head).iterator; outer.step(); step() }
      else Iterator.empty.step()
    }
    
    override def dup: Iterator[B] = new FlatMapContainer[A, B](outer.dup, f, inner.dup)
  }
  
  final class Filter[+A](self: Iterator[A], p: A => Boolean) extends Iterator[A] {
    @tailrec override def isEmpty: Boolean =
      self.isEmpty || !p(self.head) || { self.step(); isEmpty }
    
    @tailrec override def head: A = {
      val x = self.head
      if (p(x)) x
      else { self.step(); head }
    }
    
    override def step(): Unit = self.step()
    
    override def dup: Iterator[A] = new Filter[A](self.dup, p)
  }
  
  final class DropWhile[+A] private
      (self: Iterator[A], p: A => Boolean, private[this] var iterating: Boolean)
    extends Iterator[A] {
    
    def this(self: Iterator[A], p: A => Boolean) = this(self, p, false)
    
    @tailrec override def isEmpty: Boolean =
      self.isEmpty || (!iterating && (if (p(self.head)) { self.step(); isEmpty } else { iterating = true; false }))
    
    @tailrec override def head: A = {
      if (iterating) self.head
      else {
        val x = self.head
        if (p(x)) { self.step(); head }
        else { iterating = true; x }
      }
    }
    
    @tailrec override def step() {
      if (iterating) self.step()
      else if (p(self.head)) { self.step(); step() }
      else { iterating = true; self.step() }
    }
    
    override def dup: Iterator[A] = new DropWhile[A](self.dup, p, iterating)
  }
  
  final class TakeWhile[+A] private
      (self: Iterator[A], p: A => Boolean, private[this] var iterating: Boolean)
    extends Iterator[A] {
    
    def this(self: Iterator[A], p: A => Boolean) = this(self, p, true)
    
    override def isEmpty: Boolean =
      !iterating || self.isEmpty || !p(self.head) || { iterating = false; true }
    
    @tailrec override def head: A = {
      if (iterating) {
        val x = self.head
        if (p(x)) x
        else { iterating = false; head }
      }
      else Iterator.empty.head
    }
    
    @tailrec override def step() {
      if (iterating) {
        if (p(self.head)) self.step()
        else { iterating = false; step() }
      }
      else Iterator.empty.step()
    }
    
    override def dup: Iterator[A] = new TakeWhile[A](self.dup, p, iterating)
  }
  
  final class Drop[+A] private
      (self: Iterator[A], lower: Int, private[this] var index: Int)
    extends Iterator[A] {
    
    def this(self: Iterator[A], lower: Int) = this(self, lower, 0)
    
    @tailrec override def isEmpty: Boolean =
      self.isEmpty || (index < lower && { self.step(); index += 1; isEmpty })
    
    @tailrec override def head: A = {
      if (index < lower) { self.step(); index += 1; head }
      else self.head
    }
    
    @tailrec override def step() {
      if (index < lower) { self.step(); index += 1; step() }
      else self.step()
    }
    
    override def dup: Iterator[A] = new Drop[A](self.dup, lower, index)
  }
  
  final class Take[+A] private
      (self: Iterator[A], upper: Int, private[this] var index: Int)
    extends Iterator[A] {
    
    def this(self: Iterator[A], upper: Int) = this(self, upper, 0)
    
    override def isEmpty: Boolean =
      index >= upper || self.isEmpty
    
    override def head: A = {
      if (index < upper) self.head
      else Iterator.empty.head
    }
    
    override def step() {
      if (index < upper) { self.step(); index += 1 }
      else Iterator.empty.step()
    }
    
    override def dup: Iterator[A] = new Take[A](self.dup, upper, index)
  }
  
  final class Slice[+A] private
      (self: Iterator[A], lower: Int, upper: Int, private[this] var index: Int)
    extends Iterator[A] {
    
    def this(self: Iterator[A], lower: Int, upper: Int) =
      this(self, scala.math.max(0, lower), scala.math.max(scala.math.max(0, lower), upper), 0)
    
    @tailrec override def isEmpty: Boolean =
      index >= upper || self.isEmpty || (index < lower && { self.step(); index += 1; isEmpty })
    
    @tailrec override def head: A = {
      if (index < lower) { self.step(); index += 1; head }
      else if (index < upper) self.head
      else Iterator.empty.head
    }
    
    @tailrec override def step() {
      if (index < lower) { self.step(); index += 1; step() }
      else if (index < upper) self.step()
      else Iterator.empty.step()
    }
    
    override def dup: Iterator[A] = new Slice[A](self.dup, lower, upper, index)
  }
  
  final class Zip[+A, +B](xs: Iterator[A], ys: Iterator[B]) extends Iterator[(A, B)] {
    override def isEmpty: Boolean = xs.isEmpty || ys.isEmpty
    
    override def head: (A, B) = (xs.head, ys.head)
    
    override def step() {
      xs.step()
      ys.step()
    }
    
    override def dup: Iterator[(A, B)] = new Zip[A, B](xs.dup, ys.dup)
  }
  
  final class ++ [+A] private
      (xs: Iterator[A], ys: Iterator[A], private[this] var segment: Int)
    extends Iterator[A] {
    
    def this(xs: Iterator[A], ys: Iterator[A]) = this(xs, ys, 0)
    
    @tailrec override def isEmpty: Boolean = segment match {
      case 0 => xs.isEmpty && { segment = 1; isEmpty }
      case 1 => ys.isEmpty
    }
    
    @tailrec override def head: A = segment match {
      case 0 => if (!xs.isEmpty) xs.head else { segment = 1; head }
      case 1 => ys.head
    }
    
    @tailrec override def step(): Unit = segment match {
      case 0 => if (!xs.isEmpty) xs.step else { segment = 1; step() }
      case 1 => ys.step()
    }
    
    override def dup: Iterator[A] = new ++ [A](xs.dup, ys.dup, segment)
  }
}
