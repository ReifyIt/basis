/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

private[basis] object ContainerMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def deconstruct[A : c.WeakTypeTag](c: Context): c.Expr[Container[A]] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    c.Expr[Container[A]](self)
  }
  
  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val gen = self.splice.iterator
      while (gen.hasNext) f.splice(gen.next())
    }
  }
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val gen = self.splice.iterator
      var r = z.splice
      while (gen.hasNext) r = op.splice(r, gen.next())
      r
    }
  }
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val gen = self.splice.iterator
      if (!gen.hasNext) throw new java.lang.UnsupportedOperationException
      var r = gen.next(): B
      while (gen.hasNext) r = op.splice(r, gen.next())
      r
    }
  }
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Option[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val gen = self.splice.iterator
      if (gen.hasNext) {
        var r = gen.next(): B
        while (gen.hasNext) r = op.splice(r, gen.next())
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
      val gen = self.splice.iterator
      var r = None: Option[A]
      while (gen.hasNext && r.isEmpty) {
        val x = gen.next()
        if (p.splice(x)) r = Some(x)
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
      val gen = self.splice.iterator
      var r = true
      while (gen.hasNext && r) if (!p.splice(gen.next())) r = false
      r
    }
  }
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val gen = self.splice.iterator
      var r = false
      while (gen.hasNext && !r) if (p.splice(gen.next())) r = true
      r
    }
  }
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val gen = self.splice.iterator
      var t = 0
      while (gen.hasNext) if (p.splice(gen.next())) t += 1
      t
    }
  }
  
  def select[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[scala.PartialFunction[A, B]])
    : c.Expr[Option[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val gen = self.splice.iterator
      var r = None: Option[B]
      while (gen.hasNext && r.isEmpty) {
        val x = gen.next()
        if (q.splice.isDefinedAt(x)) r = Some(q.splice(x))
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
      val gen = self.splice.iterator
      while (gen.hasNext) {
        val x = gen.next()
        if (q.splice.isDefinedAt(x)) b += q.splice(x)
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
      val gen = self.splice.iterator
      while (gen.hasNext) b += f.splice(gen.next())
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def flatMap[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Container[B]])
      (buffer: c.Expr[Buffer[Nothing, B]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, B]#State] {
      val b = buffer.splice
      val outer = self.splice.iterator
      while (outer.hasNext) {
        val inner = f.splice(outer.next()).iterator
        while (inner.hasNext) b += inner.next()
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
      val gen = self.splice.iterator
      while (gen.hasNext) {
        val x = gen.next()
        if (p.splice(x)) b += x
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
      val gen = self.splice.iterator
      while (gen.hasNext && { val x = gen.next(); p.splice(x) || { b += x; false } }) ()
      while (gen.hasNext) b += gen.next()
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
      val gen = self.splice.iterator
      while (gen.hasNext && { val x = gen.next(); p.splice(x) && { b += x; true } }) ()
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
      val gen = self.splice.iterator
      while (gen.hasNext && { val x = gen.next(); (p.splice(x) && { a += x; true }) || { b += x; false } }) ()
      while (gen.hasNext) b += gen.next()
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
      val gen = self.splice.iterator
      var i = 0
      val n = lower.splice
      while (i < n && gen.hasNext) { gen.next(); i += 1 }
      while (gen.hasNext) b += gen.next()
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
      val gen = self.splice.iterator
      var i = 0
      val n = upper.splice
      while (i < n && gen.hasNext) { b += gen.next(); i += 1 }
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
      val gen = self.splice.iterator
      var i = 0
      var n = lower.splice
      while (i < n && gen.hasNext) { gen.next(); i += 1 }
      n = upper.splice
      while (i < n && gen.hasNext) { b += gen.next(); i += 1 }
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[Container[B]])
      (buffer: c.Expr[Buffer[Nothing, (A, B)]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, (A, B)]#State] {
      val b = buffer.splice
      val xs = self.splice.iterator
      val ys = that.splice.iterator
      while (xs.hasNext && ys.hasNext) b += ((xs.next(), ys.next()))
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
  
  def ++ [A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[Container[B]])
      (buffer: c.Expr[Buffer[Nothing, B]])
    : c.Expr[buffer.value.State] = {
    val self = deconstruct[A](c)
    c.universe.reify[Buffer[Nothing, B]#State] {
      val b = buffer.splice
      val xs = self.splice.iterator
      while (xs.hasNext) b += xs.next()
      val ys = that.splice.iterator
      while (ys.hasNext) b += ys.next()
      b.check
    }.asInstanceOf[c.Expr[buffer.value.State]]
  }
}
