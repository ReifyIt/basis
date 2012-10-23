/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._

private[basis] object ListMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[A](c: Context)(xs: c.Expr[A]*): c.Expr[List[A]] = {
    import c.universe._
    c.Expr(
      xs.foldRight
        (Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "container"), "Nil"): c.Tree)
        ((x: c.Expr[A], xs: c.Tree) => Apply(Select(xs, "$colon$colon"), x.tree :: Nil))
    ) (WeakTypeTag.Nothing)
  }
  
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
      while (r.isEmpty && !xs.isEmpty) {
        val x = xs.head
        if (p.splice(x)) r = Some(x)
        else xs = xs.tail
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
      while (r && !xs.isEmpty) {
        if (!p.splice(xs.head)) r = false
        else xs = xs.tail
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
      while (!r && !xs.isEmpty) {
        if (p.splice(xs.head)) r = true
        else xs = xs.tail
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
      (q: c.Expr[scala.PartialFunction[A, B]])
    : c.Expr[Option[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      var xs = self.splice
      var r = None: Option[B]
      while (r.isEmpty && !xs.isEmpty) {
        val x = xs.head
        if (q.splice.isDefinedAt(x)) r = Some(q.splice(x))
        else xs = xs.tail
      }
      r
    }
  }
  
  def collect[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[scala.PartialFunction[A, B]])
    : c.Expr[List[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val b = new List.Builder[B]
      var xs = self.splice
      while (!xs.isEmpty) {
        val x = xs.head
        if (q.splice.isDefinedAt(x)) b += q.splice(x)
        xs = xs.tail
      }
      b.state
    }
  }
  
  def map[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
    : c.Expr[List[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val b = new List.Builder[B]
      var xs = self.splice
      while (!xs.isEmpty) {
        b += f.splice(xs.head)
        xs = xs.tail
      }
      b.state
    }
  }
  
  def flatMap[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => List[B]])
    : c.Expr[List[B]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val b = new List.Builder[B]
      var xs = self.splice
      while (!xs.isEmpty) {
        var ys = f.splice(xs.head)
        while (!ys.isEmpty) {
          b += ys.head
          ys = ys.tail
        }
        xs = xs.tail
      }
      b.state
    }
  }
  
  def filter[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[List[A]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val b = new List.Builder[A]
      var xs = self.splice
      while (!xs.isEmpty) {
        val x = xs.head
        if (p.splice(x)) b += x
        xs = xs.tail
      }
      b.state
    }
  }
  
  def dropWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[List[A]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      var xs = self.splice
      while (!xs.isEmpty && p.splice(xs.head)) xs = xs.tail
      xs
    }
  }
  
  def takeWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[List[A]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val b = new List.Builder[A]
      var xs = self.splice
      while (!xs.isEmpty && p.splice(xs.head)) {
        b += xs.head
        xs = xs.tail
      }
      b.state
    }
  }
  
  def span[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[(List[A], List[A])] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val b = new List.Builder[A]
      var xs = self.splice
      while (!xs.isEmpty && p.splice(xs.head)) {
        b += xs.head
        xs = xs.tail
      }
      (b.state, xs)
    }
  }
}
