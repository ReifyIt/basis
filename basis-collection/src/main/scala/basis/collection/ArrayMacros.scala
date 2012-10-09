/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

private[basis] object ArrayMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def deconstruct[A : c.WeakTypeTag](c: Context): c.Expr[Array[A]] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    c.Expr[Array[A]](self)
  }
  
  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var i = 0
      val n = xs.length
      while (i < n) {
        f.splice(xs(i))
        i += 1
      }
    }
  }
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var r = z.splice
      var i = 0
      val n = xs.length
      while (i < n) {
        r = op.splice(r, xs(i))
        i += 1
      }
      r
    }
  }
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      val n = xs.length
      if (n == 0) throw new java.lang.UnsupportedOperationException
      var r = xs(0): B
      var i = 1
      while (i < n) {
        r = op.splice(r, xs(i))
        i += 1
      }
      r
    }
  }
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Option[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      val n = xs.length
      if (n > 0) {
        var r = xs(0): B
        var i = 1
        while (i < n) {
          r = op.splice(r, xs(i))
          i += 1
        }
        Some(r)
      }
      else None
    }
  }
  
  def foldRight[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(A, B) => B])
    : c.Expr[B] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var r = z.splice
      var i = xs.length - 1
      while (i >= 0) {
        r = op.splice(xs(i), r)
        i -= 1
      }
      r
    }
  }
  
  def reduceRight[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(A, B) => B])
    : c.Expr[B] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var i = xs.length - 1
      if (i < 0) throw new java.lang.UnsupportedOperationException
      var r = xs(i): B
      i -= 1
      while (i >= 0) {
        r = op.splice(xs(i), r)
        i -= 1
      }
      r
    }
  }
  
  def reduceRightOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(A, B) => B])
    : c.Expr[Option[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var i = xs.length - 1
      if (i >= 0) {
        var r = xs(i): B
        i -= 1
        while (i >= 0) {
          r = op.splice(xs(i), r)
          i -= 1
        }
        Some(r)
      }
      else None
    }
  }
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Option[A]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var r = None: Option[A]
      var i = 0
      val n = xs.length
      while (i < n && r.isEmpty) {
        val x = xs(i)
        if (p.splice(x)) r = Some(x)
        i += 1
      }
      r
    }
  }
  
  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var r = true
      var i = 0
      val n = xs.length
      while (i < n && r) {
        if (!p.splice(xs(i))) r = false
        i += 1
      }
      r
    }
  }
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var r = false
      var i = 0
      val n = xs.length
      while (i < n && !r) {
        if (p.splice(xs(i))) r = true
        i += 1
      }
      r
    }
  }
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var t = 0
      var i = 0
      val n = xs.length
      while (i < n) {
        if (p.splice(xs(i))) t += 1
        i += 1
      }
      t
    }
  }
  
  def select[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[scala.PartialFunction[A, B]])
    : c.Expr[Option[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val xs = self.splice
      var r = None: Option[B]
      var i = 0
      val n = xs.length
      while (i < n && r.isEmpty) {
        val x = xs(i)
        if (q.splice.isDefinedAt(x)) r = Some(q.splice(x))
        i += 1
      }
      r
    }
  }
  
  def collect[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[scala.PartialFunction[A, B]])
      (buffer: c.Expr[Buffer[Nothing, B]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, B]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = 0
      val n = xs.length
      while (i < n) {
        val x = xs(i)
        if (q.splice.isDefinedAt(x)) b += q.splice(x)
        i += 1
      }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def map[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (buffer: c.Expr[Buffer[Nothing, B]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, B]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = 0
      val n = xs.length
      b.expect(n)
      while (i < n) {
        b += f.splice(xs(i))
        i += 1
      }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def flatMap[A : c.WeakTypeTag, B: c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Array[B]])
      (buffer: c.Expr[Buffer[Nothing, B]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, B]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = 0
      val m = xs.length
      while (i < m) {
        val ys = f.splice(xs(i))
        var j = 0
        val n = ys.length
        while (j < n) {
          b += ys(j)
          j += 1
        }
        i += 1
      }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def filter[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (buffer: c.Expr[Buffer[Nothing, A]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, A]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = 0
      val n = xs.length
      while (i < n) {
        val x = xs(i)
        if (p.splice(x)) b += x
        i += 1
      }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def dropWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (buffer: c.Expr[Buffer[Nothing, A]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, A]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = 0
      val n = xs.length
      while (i < n && { val x = xs(i); i += 1; p.splice(x) || { b += x; false } }) ()
      while (i < n) { b += xs(i); i += 1 }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def takeWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (buffer: c.Expr[Buffer[Nothing, A]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, A]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = 0
      val n = xs.length
      while (i < n && { val x = xs(i); i += 1; p.splice(x) && { b += x; true } }) ()
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def span[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builderA: c.Expr[Buffer[Nothing, A]], builderB: c.Expr[Buffer[Nothing, A]])
    : c.Expr[(builderA.value.State, builderB.value.State)] = {
    val self = deconstruct[A](c)
    c.universe.reify[(Buffer[Nothing, A]#State, Buffer[Nothing, A]#State)] {
      val a = builderA.splice
      val b = builderB.splice
      val xs = self.splice
      var i = 0
      val n = xs.length
      while (i < n && { val x = xs(i); i += 1; (p.splice(x) && { a += x; true }) || { b += x; false } }) ()
      while (i < n) { b += xs(i); i += 1 }
      (a.check, b.check)
    }.asInstanceOf[c.Expr[(builderA.value.State, builderB.value.State)]]
  }
  
  def drop[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (buffer: c.Expr[Buffer[Nothing, A]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, A]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = scala.math.max(0, lower.splice)
      val n = xs.length
      while (i < n) { b += xs(i); i += 1 }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def take[A : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (buffer: c.Expr[Buffer[Nothing, A]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, A]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = 0
      val n = scala.math.min(upper.splice, xs.length)
      while (i < n) { b += xs(i); i += 1 }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def slice[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (buffer: c.Expr[Buffer[Nothing, A]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, A]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = scala.math.max(0, lower.splice)
      val n = scala.math.min(upper.splice, xs.length)
      while (i < n) { b += xs(i); i += 1 }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def reverse[A : c.WeakTypeTag]
      (c: Context)
      (buffer: c.Expr[Buffer[Nothing, A]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, A]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = xs.length - 1
      while (i >= 0) { b += xs(i); i -= 1 }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def ++ [A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[Array[B]])
      (buffer: c.Expr[Buffer[Nothing, B]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, B]#State] {
      val b = buffer.splice
      val xs = self.splice
      var i = 0
      var n = xs.length
      while (i < n) { b += xs(i); i += 1 }
      val ys = that.splice
      i = 0
      n = ys.length
      while (i < n) { b += ys(i); i += 1 }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
}
