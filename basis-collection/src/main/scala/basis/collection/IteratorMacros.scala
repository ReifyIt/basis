/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

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
      val iter = self.splice
      while (!iter.isEmpty) {
        f.splice(iter.head)
        iter.step()
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
      val iter = self.splice
      var r = z.splice
      while (!iter.isEmpty) {
        r = op.splice(r, iter.head)
        iter.step()
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
      val iter = self.splice
      if (iter.isEmpty) throw new java.lang.UnsupportedOperationException
      var r = iter.head: B
      iter.step()
      while (!iter.isEmpty) {
        r = op.splice(r, iter.head)
        iter.step()
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
      val iter = self.splice
      if (iter.isEmpty) None
      else {
        var r = iter.head: B
        iter.step()
        while (!iter.isEmpty) {
          r = op.splice(r, iter.head)
          iter.step()
        }
        Some(r)
      }
    }
  }
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Option[A]] = {
    val self = deconstruct[A](c)
    c.universe.reify {
      val iter = self.splice
      var r = None: Option[A]
      while (r.isEmpty && !iter.isEmpty) {
        val x = iter.head
        if (p.splice(x)) r = Some(x)
        else iter.step()
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
      val iter = self.splice
      var r = true
      while (r && !iter.isEmpty) {
        if (!p.splice(iter.head)) r = false
        else iter.step()
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
      val iter = self.splice
      var r = false
      while (!r && !iter.isEmpty) {
        if (p.splice(iter.head)) r = true
        else iter.step()
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
      val iter = self.splice
      var t = 0
      while (!iter.isEmpty) {
        if (p.splice(iter.head)) t += 1
        iter.step()
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
      val iter = self.splice
      var r = None: Option[B]
      while (r.isEmpty && !iter.isEmpty) {
        val x = iter.head
        if (q.splice.isDefinedAt(x)) r = Some(q.splice(x))
        else iter.step()
      }
      r
    }
  }
}
