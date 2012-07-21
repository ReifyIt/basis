/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Near[+A] extends Any with Such[A] {
  def length: Int
  
  def apply(index: Int): A
  
  override def iterator: Next[A] = new Near.Iterator(this, 0, length)
  
  override def foreach[U](f: A => U) {
    var i = 0
    val until = length
    while (i < until) {
      f(apply(i))
      i += 1
    }
  }
}

object Near {
  import scala.language.implicitConversions
  
  private[basis] final class Iterator[+A](self: Near[A], lower: Int, upper: Int) extends Next[A] {
    private[this] var index: Int = lower
    override def hasNext: Boolean = index < upper
    override def next(): A = {
      if (index >= upper) Next.Nada.next()
      val x = self.apply(index)
      index += 1
      x
    }
  }
  
  private[basis] final class ReverseIterator[+A](self: Near[A], upper: Int, lower: Int) extends Next[A] {
    private[this] var index: Int = upper - 1
    override def hasNext: Boolean = index >= lower
    override def next(): A = {
      if (index < lower) Next.Nada.next()
      val x = self.apply(index)
      index -= 1
      x
    }
  }
  
  @inline implicit def ForNear[A](self: Near[A]): ForNear[self.From, A] =
    new ForNear[self.From, A](self)
  
  final class ForNear[From, A](val __ : Near[A]) extends AnyVal {
    import __.{length, apply}
    
    @inline def select[B](q: PartialFunction[A, B]): Option[B] = {
      var i = 0
      val until = length
      while (i < until) {
        val x = apply(i)
        if (q.isDefinedAt(x)) return Some(q(x))
        i += 1
      }
      None
    }
    
    @inline def fold[B >: A](z: B)(op: (B, B) => B): B = {
      var result = z
      var i = 0
      val until = length
      while (i < until) {
        result = op(result, apply(i))
        i += 1
      }
      result
    }
    
    @inline def reduce[B >: A](op: (B, B) => B): B = {
      val until = length
      if (until == 0) throw new UnsupportedOperationException
      var result: B = apply(0)
      var i = 1
      while (i < until) {
        result = op(result, apply(i))
        i += 1
      }
      result
    }
    
    @inline def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
      val until = length
      if (until == 0) return None
      var result: B = apply(0)
      var i = 1
      while (i < until) {
        result = op(result, apply(i))
        i += 1
      }
      Some(result)
    }
    
    @inline def foldLeft[B](z: B)(op: (B, A) => B): B = {
      var result = z
      var i = 0
      val until = length
      while (i < until) {
        result = op(result, apply(i))
        i += 1
      }
      result
    }
    
    @inline def reduceLeft[B >: A](op: (B, A) => B): B = {
      val until = length
      if (until == 0) throw new UnsupportedOperationException
      var result: B = apply(0)
      var i = 1
      while (i < until) {
        result = op(result, apply(i))
        i += 1
      }
      result
    }
    
    @inline def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
      val until = length
      if (until == 0) return None
      var result: B = apply(0)
      var i = 1
      while (i < until) {
        result = op(result, apply(i))
        i += 1
      }
      Some(result)
    }
    
    @inline def foldRight[B](z: B)(op: (A, B) => B): B = {
      var result = z
      var i = length - 1
      while (i >= 0) {
        result = op(apply(i), result)
        i -= 1
      }
      result
    }
    
    @inline def reduceRight[B >: A](op: (A, B) => B): B = {
      var i = length - 1
      if (i < 0) throw new UnsupportedOperationException
      var result: B = apply(i)
      i -= 1
      while (i >= 0) {
        result = op(apply(i), result)
        i -= 1
      }
      result
    }
    
    @inline def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = {
      var i = length - 1
      if (i < 0) return None
      var result: B = apply(i)
      i -= 1
      while (i >= 0) {
        result = op(apply(i), result)
        i -= 1
      }
      Some(result)
    }
    
    @inline def find(p: A => Boolean): Option[A] = {
      var i = 0
      val until = length
      while (i < until) {
        val x = apply(i)
        if (p(x)) return Some(x)
        i += 1
      }
      None
    }
    
    @inline def forall(p: A => Boolean): Boolean = {
      var i = 0
      val until = length
      while (i < until) {
        if (!p(apply(i))) return false
        i += 1
      }
      true
    }
    
    @inline def exists(p: A => Boolean): Boolean = {
      var i = 0
      val until = length
      while (i < until) {
        if (p(apply(i))) return true
        i += 1
      }
      false
    }
    
    @inline def count(p: A => Boolean): Int = {
      var total = 0
      var i = 0
      val until = length
      while (i < until) {
        if (p(apply(i))) total += 1
        i += 1
      }
      total
    }
    
    @inline def map[B](f: A => B)(implicit make: Make[From, B]): make.What = {
      var i = 0
      val until = length
      make.expect(until)
      while (i < until) {
        make += f(apply(i))
        i += 1
      }
      make.result
    }
    
    @inline def flatMap[B](f: A => Near[B])(implicit make: Make[From, B]): make.What = {
      var i = 0
      val until = length
      while (i < until) {
        val that = f(apply(i))
        var j = 0
        val limit = that.length
        while (j < limit) {
          make += that.apply(j)
          j += 1
        }
        i += 1
      }
      make.result
    }
    
    @inline def filter(p: A => Boolean)(implicit make: Make[From, A]): make.What = {
      var i = 0
      val until = length
      while (i < until) {
        val x = apply(i)
        if (p(x)) make += x
        i += 1
      }
      make.result
    }
    
    @inline def collect[B](q: PartialFunction[A, B])(implicit make: Make[From, B]): make.What = {
      var i = 0
      val until = length
      while (i < until) {
        val x = apply(i)
        if (q.isDefinedAt(x)) make += q(x)
        i += 1
      }
      make.result
    }
    
    @inline def dropWhile(p: A => Boolean)(implicit make: Make[From, A]): make.What = {
      var i = 0
      var x = null.asInstanceOf[A]
      val until = length
      while (i < until && { x = apply(i); p(x) }) i += 1
      if (i < until) { make += x; i += 1 }
      while (i < until) { make += apply(i); i += 1 }
      make.result
    }
    
    @inline def takeWhile(p: A => Boolean)(implicit make: Make[From, A]): make.What = {
      var i = 0
      var x = null.asInstanceOf[A]
      val until = length
      while (i < until && { x = apply(i); p(x) }) { make += x; i += 1 }
      make.result
    }
    
    @inline def span(p: A => Boolean)(implicit makeA: Make[From, A], makeB: Make[From, A]): (makeA.What, makeB.What) = {
      var i = 0
      var x = null.asInstanceOf[A]
      val until = length
      while (i < until && { x = apply(i); p(x) }) { makeA += x; i += 1 }
      if (i < until) { makeB += x; i += 1 }
      while (i < until) { makeB += apply(i); i += 1 }
      (makeA.result, makeB.result)
    }
    
    def drop(lower: Int)(implicit make: Make[From, A]): make.What = {
      var i = math.max(0, lower)
      val until = length
      while (i < until) {
        make += apply(i)
        i += 1
      }
      make.result
    }
    
    def take(upper: Int)(implicit make: Make[From, A]): make.What = {
      var i = 0
      val until = math.min(upper, length)
      while (i < until) {
        make += apply(i)
        i += 1
      }
      make.result
    }
    
    def slice(lower: Int, upper: Int)(implicit make: Make[From, A]): make.What = {
      var i = math.max(0, lower)
      val until = math.min(upper, length)
      while (i < until) {
        make += apply(i)
        i += 1
      }
      make.result
    }
    
    def reverse(implicit make: Make[From, A]): make.What = {
      var i = length - 1
      while (i >= 0) {
        make += apply(i)
        i -= 1
      }
      make.result
    }
  }
}
