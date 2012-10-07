/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

private[basis] object ListMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def deconstruct[A : c.WeakTypeTag](c: Context): c.Expr[List[A]] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    c.Expr[List[A]](self)
  }
  
  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      var xs = self.splice
      while (!xs.isEmpty) {
        f.splice(xs.head)
        xs = xs.tail
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
      var xs = self.splice
      var r = z.splice
      while (!xs.isEmpty) {
        r = op.splice(r, xs.head)
        xs = xs.tail
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
      var xs = self.splice
      if (xs.isEmpty) throw new java.lang.UnsupportedOperationException
      var r = xs.head: B
      xs = xs.tail
      while (!xs.isEmpty) {
        r = op.splice(r, xs.head)
        xs = xs.tail
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
      var xs = self.splice
      if (!xs.isEmpty) {
        var r = xs.head: B
        xs = xs.tail
        while (!xs.isEmpty) {
          r = op.splice(r, xs.head)
          xs = xs.tail
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
      var xs = self.splice
      var r = None: Option[A]
      while (!xs.isEmpty && r.isEmpty) {
        val x = xs.head
        if (p.splice(x)) r = Some(x)
        xs = xs.tail
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
      var xs = self.splice
      var r = true
      while (!xs.isEmpty && r) {
        if (!p.splice(xs.head)) r = false
        xs = xs.tail
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
      var xs = self.splice
      var r = false
      while (!xs.isEmpty && !r) {
        if (p.splice(xs.head)) r = true
        xs = xs.tail
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
      var xs = self.splice
      var t = 0
      while (!xs.isEmpty) {
        if (p.splice(xs.head)) t += 1
        xs = xs.tail
      }
      t
    }
  }
  
  def select[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Option[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      var xs = self.splice
      var r = None: Option[B]
      while (!xs.isEmpty && r.isEmpty) {
        val x = xs.head
        if (q.splice.isDefinedAt(x)) r = Some(q.splice(x))
        xs = xs.tail
      }
      r
    }
  }
  
  def collect[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (buffer: c.Expr[Buffer[Nothing, B]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, B]#State] {
      val b = buffer.splice
      var xs = self.splice
      while (!xs.isEmpty) {
        val x = xs.head
        if (q.splice.isDefinedAt(x)) b += q.splice(x)
        xs = xs.tail
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
      var xs = self.splice
      while (!xs.isEmpty) {
        b += f.splice(xs.head)
        xs = xs.tail
      }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def flatMap[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => List[B]])
      (buffer: c.Expr[Buffer[Nothing, B]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, B]#State] {
      val b = buffer.splice
      var xs = self.splice
      while (!xs.isEmpty) {
        var ys = f.splice(xs.head)
        while (!ys.isEmpty) {
          b += ys.head
          ys = ys.tail
        }
        xs = xs.tail
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
      var xs = self.splice
      while (!xs.isEmpty) {
        val x = xs.head
        if (p.splice(x)) b += x
        xs = xs.tail
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
      var xs = self.splice
      while (!xs.isEmpty && { val x = xs.head; xs = xs.tail; p.splice(x) || { b += x; false } }) ()
      while (!xs.isEmpty) { b += xs.head; xs = xs.tail }
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
      var xs = self.splice
      while (!xs.isEmpty && { val x = xs.head; xs = xs.tail; p.splice(x) && { b += x; true } }) ()
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
      var xs = self.splice
      while (!xs.isEmpty && { val x = xs.head; xs = xs.tail; (p.splice(x) && { a += x; true }) || { b += x; false } }) ()
      while (!xs.isEmpty) { b += xs.head; xs = xs.tail }
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
      var xs = self.splice
      var i = 0
      val n = lower.splice
      while (i < n && !xs.isEmpty) { xs = xs.tail; i += 1 }
      while (!xs.isEmpty) { b += xs.head; xs = xs.tail }
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
      var xs = self.splice
      var i = 0
      val n = upper.splice
      while (i < n && !xs.isEmpty) { b += xs.head; xs = xs.tail; i += 1 }
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
      var xs = self.splice
      var i = 0
      var n = lower.splice
      while (i < n && !xs.isEmpty) { xs = xs.tail; i += 1 }
      n = upper.splice
      while (i < n && !xs.isEmpty) { b += xs.head; xs = xs.tail; i += 1 }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def ++ [A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[List[B]])
      (buffer: c.Expr[Buffer[Nothing, B]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, B]#State] {
      val b = buffer.splice
      var xs = self.splice
      while (!xs.isEmpty) { b += xs.head; xs = xs.tail }
      var ys = that.splice
      while (!ys.isEmpty) { b += ys.head; ys = ys.tail }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
}
