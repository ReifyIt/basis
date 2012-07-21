/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait More[+A] extends Any with Given[A] {
  def isEmpty: Boolean
  
  def head: A
  
  def tail: More[A]
  
  override def iterator: Next[A] = new More.Iterator[A](this)
  
  override def foreach[U](f: A => U) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }
}

object More {
  import scala.language.implicitConversions
  
  private[basis] final class Iterator[+A](private[this] var these: More[A]) extends Next[A] {
    override def hasNext: Boolean = !these.isEmpty
    override def next(): A = {
      if (these.isEmpty) Next.Nada.next()
      val x = these.head
      these = these.tail
      x
    }
  }
  
  @inline implicit def ForMore[A](self: More[A]): ForMore[self.Self, A] =
    new ForMore[self.Self, A](self)
  
  final class ForMore[From, A](val __ : More[A]) extends AnyVal {
    @inline def select[B](q: PartialFunction[A, B]): Option[B] = {
      var these = __
      while (!these.isEmpty) {
        val x = these.head
        if (q.isDefinedAt(x)) return Some(q(x))
        these = these.tail
      }
      None
    }
    
    @inline def fold[B >: A](z: B)(op: (B, B) => B): B = {
      var these = __
      var result = z
      while (!these.isEmpty) {
        result = op(result, these.head)
        these = these.tail
      }
      result
    }
    
    @inline def reduce[B >: A](op: (B, B) => B): B = {
      var these = __
      if (these.isEmpty) throw new UnsupportedOperationException
      var result: B = these.head
      these = these.tail
      while (!these.isEmpty) {
        result = op(result, these.head)
        these = these.tail
      }
      result
    }
    
    @inline def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
      var these = __
      if (these.isEmpty) return None
      var result: B = these.head
      these = these.tail
      while (!these.isEmpty) {
        result = op(result, these.head)
        these = these.tail
      }
      Some(result)
    }
    
    @inline def foldLeft[B](z: B)(op: (B, A) => B): B = {
      var these = __
      var result = z
      while (!these.isEmpty) {
        result = op(result, these.head)
        these = these.tail
      }
      result
    }
    
    @inline def reduceLeft[B >: A](op: (B, A) => B): B = {
      var these = __
      if (these.isEmpty) throw new UnsupportedOperationException
      var result: B = these.head
      these = these.tail
      while (!these.isEmpty) {
        result = op(result, these.head)
        these = these.tail
      }
      result
    }
    
    @inline def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
      var these = __
      if (these.isEmpty) return None
      var result: B = these.head
      these = these.tail
      while (!these.isEmpty) {
        result = op(result, these.head)
        these = these.tail
      }
      Some(result)
    }
    
    @inline def find(p: A => Boolean): Option[A] = {
      var these = __
      while (!these.isEmpty) {
        val x = these.head
        if (p(x)) return Some(x)
        these = these.tail
      }
      None
    }
    
    @inline def forall(p: A => Boolean): Boolean = {
      var these = __
      while (!these.isEmpty) {
        if (!p(these.head)) return false
        these = these.tail
      }
      true
    }
    
    @inline def exists(p: A => Boolean): Boolean = {
      var these = __
      while (!these.isEmpty) {
        if (p(these.head)) return true
        these = these.tail
      }
      false
    }
    
    @inline def count(p: A => Boolean): Int = {
      var these = __
      var total = 0
      while (!these.isEmpty) {
        if (p(these.head)) total += 1
        these = these.tail
      }
      total
    }
    
    @inline def map[B](f: A => B)(implicit make: Make[From, B]): make.What = {
      var these = __
      while (!these.isEmpty) {
        make += f(these.head)
        these = these.tail
      }
      make.result
    }
    
    @inline def flatMap[B](f: A => More[B])(implicit make: Make[From, B]): make.What = {
      var these = __
      while (!these.isEmpty) {
        var those = f(these.head)
        while (!those.isEmpty) {
          make += those.head
          those = those.tail
        }
        these = these.tail
      }
      make.result
    }
    
    @inline def filter(p: A => Boolean)(implicit make: Make[From, A]): make.What = {
      var these = __
      while (!these.isEmpty) {
        val x = these.head
        if (p(x)) make += x
        these = these.tail
      }
      make.result
    }
    
    @inline def collect[B](q: PartialFunction[A, B])(implicit make: Make[From, B]): make.What = {
      var these = __
      while (!these.isEmpty) {
        val x = these.head
        if (q.isDefinedAt(x)) make += q(x)
        these = these.tail
      }
      make.result
    }
    
    @inline def dropWhile(p: A => Boolean)(implicit make: Make[From, A]): make.What = {
      var these = __
      var x = null.asInstanceOf[A]
      while (!these.isEmpty && { x = these.head; p(x) }) these = these.tail
      if (!these.isEmpty) { make += x; these = these.tail }
      while (!these.isEmpty) { make += these.head; these = these.tail }
      make.result
    }
    
    @inline def takeWhile(p: A => Boolean)(implicit make: Make[From, A]): make.What = {
      var these = __
      var x = null.asInstanceOf[A]
      while (!these.isEmpty && { x = these.head; p(x) }) { make += x; these = these.tail }
      make.result
    }
    
    @inline def span(p: A => Boolean)(implicit makeA: Make[From, A], makeB: Make[From, A]): (makeA.What, makeB.What) = {
      var these = __
      var x = null.asInstanceOf[A]
      while (!these.isEmpty && { x = these.head; p(x) }) { makeA += x; these = these.tail }
      if (!these.isEmpty) { makeB += x; these = these.tail }
      while (!these.isEmpty) { makeB += these.head; these = these.tail }
      (makeA.result, makeB.result)
    }
    
    def drop(lower: Int)(implicit make: Make[From, A]): make.What = {
      var these = __
      var i = 0
      while (!these.isEmpty && i < lower) { these = these.tail; i += 1 }
      while (!these.isEmpty) { make += these.head; these = these.tail }
      make.result
    }
    
    def take(upper: Int)(implicit make: Make[From, A]): make.What = {
      var these = __
      var i = 0
      while (!these.isEmpty && i < upper) { make += these.head; these = these.tail; i += 1 }
      make.result
    }
    
    def slice(lower: Int, upper: Int)(implicit make: Make[From, A]): make.What = {
      var these = __
      var i = 0
      while (!these.isEmpty && i < lower) { these = these.tail; i += 1 }
      while (!these.isEmpty && i < upper) { make += these.head; these = these.tail; i += 1 }
      make.result
    }
  }
}
