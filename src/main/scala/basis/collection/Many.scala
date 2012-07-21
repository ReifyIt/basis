/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Many[+A] extends Any with Each[A] {
  def iterator: Next[A]
  
  override def foreach[U](f: A => U) {
    val iter = iterator
    while (iter.hasNext) f(iter.next())
  }
}

object Many {
  import scala.language.implicitConversions
  
  @inline def ForMany[A](self: Many[A]): ForMany[self.From, A] =
    new ForMany[self.From, A](self)
  
  final class ForMany[From, A](val __ : Many[A]) extends AnyVal {
    import __.iterator
    
    @inline def select[B](q: PartialFunction[A, B]): Option[B] = {
      val iter = iterator
      while (iter.hasNext) {
        val x = iter.next()
        if (q.isDefinedAt(x)) return Some(q(x))
      }
      None
    }
    
    @inline def fold[B >: A](z: B)(op: (B, B) => B): B = {
      val iter = iterator
      var result = z
      while (iter.hasNext) result = op(result, iter.next())
      result
    }
    
    @inline def reduce[B >: A](op: (B, B) => B): B = {
      val iter = iterator
      if (!iter.hasNext) throw new UnsupportedOperationException
      var result: B = iter.next()
      while (iter.hasNext) result = op(result, iter.next())
      result
    }
    
    @inline def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
      val iter = iterator
      if (iter.hasNext) return None
      var result: B = iter.next()
      while (iter.hasNext) result = op(result, iter.next())
      Some(result)
    }
    
    @inline def find(p: A => Boolean): Option[A] = {
      val iter = iterator
      while (iter.hasNext) { val x = iter.next(); if (p(x)) return Some(x) }
      None
    }
    
    @inline def forall(p: A => Boolean): Boolean = {
      val iter = iterator
      while (iter.hasNext) if (!p(iter.next())) return false
      true
    }
    
    @inline def exists(p: A => Boolean): Boolean = {
      val iter = iterator
      while (iter.hasNext) if (p(iter.next())) return true
      false
    }
    
    @inline def count(p: A => Boolean): Int = {
      val iter = iterator
      var total = 0
      while (iter.hasNext) if (p(iter.next())) total += 1
      total
    }
    
    @inline def map[B](f: A => B)(implicit make: Make[From, B]): make.What = {
      val iter = iterator
      while (iter.hasNext) make += f(iter.next())
      make.result
    }
    
    @inline def flatMap[B](f: A => Many[B])(implicit make: Make[From, B]): make.What = {
      val these = iterator
      while (these.hasNext) {
        val those = f(these.next()).iterator
        while (those.hasNext) make += those.next()
      }
      make.result
    }
    
    @inline def filter(p: A => Boolean)(implicit make: Make[From, A]): make.What = {
      val iter = iterator
      while (iter.hasNext) { val x = iter.next(); if (p(x)) make += x }
      make.result
    }
    
    def withFilter(p: A => Boolean): Many[A] = new WithFilter(__, p)
    
    @inline def collect[B](q: PartialFunction[A, B])(implicit make: Make[From, B]): make.What = {
      val iter = iterator
      while (iter.hasNext) { val x = iter.next(); if (q.isDefinedAt(x)) make += q(x) }
      make.result
    }
    
    def zip[B](that: Many[B]): Many[(A, B)] = new Zip(__, that)
  }
  
  private[basis] final class WithFilter[+A](self: Many[A], p: A => Boolean) extends Many[A] {
    override def iterator: Next[A] = self.iterator filter p
    override def foreach[U](f: A => U): Unit = self.foreach(new Analysis.Filter(f, p))
  }
  
  private[basis] final class Zip[+A, +B](these: Many[A], those: Many[B]) extends Many[(A, B)] {
    override def iterator: Next[(A, B)] = these.iterator zip those.iterator
  }
}
