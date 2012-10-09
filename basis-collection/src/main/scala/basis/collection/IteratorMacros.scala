/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

private[basis] object IteratorMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def deconstruct[A : c.WeakTypeTag](c: Context): c.Expr[Iterator[A]] = {
    import c.universe._
    val Apply(_, self :: Nil) = c.prefix.tree
    c.Expr[Iterator[A]](self)
  }
  
  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val gen = self.splice
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
      val gen = self.splice
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
      val gen = self.splice
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
      val gen = self.splice
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
      val gen = self.splice
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
      val gen = self.splice
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
      val gen = self.splice
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
      val gen = self.splice
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
      val gen = self.splice
      var r = None: Option[B]
      while (gen.hasNext && r.isEmpty) {
        val x = gen.next()
        if (q.splice.isDefinedAt(x)) r = Some(q.splice(x))
      }
      r
    }
  }
}
