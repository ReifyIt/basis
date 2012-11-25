/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential
package general

import traversable._

/** General container operations.
  * 
  * @groupprio  Traversing  -3
  * @groupprio  Reducing    -2
  * @groupprio  Querying    -1
  * 
  * @define collection  container
  */
trait ContainerOps[+A, +From] extends Any with CollectionOps[A, From] {
  override def foreach[U](f: A => U): Unit =
    macro ContainerMacros.foreach[A, U]
  
  override def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro ContainerMacros.foldLeft[A, B]
  
  override def reduce[B >: A](op: (B, B) => B): B =
    macro ContainerMacros.reduceLeft[A, B]
  
  override def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro ContainerMacros.reduceLeftOption[A, B]
  
  override def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro ContainerMacros.foldLeft[A, B]
  
  override def reduceLeft[B >: A](op: (B, A) => B): B =
    macro ContainerMacros.reduceLeft[A, B]
  
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro ContainerMacros.reduceLeftOption[A, B]
  
  override def find(p: A => Boolean): Option[A] =
    macro ContainerMacros.find[A]
  
  override def forall(p: A => Boolean): Boolean =
    macro ContainerMacros.forall[A]
  
  override def exists(p: A => Boolean): Boolean =
    macro ContainerMacros.exists[A]
  
  override def count(p: A => Boolean): Int =
    macro ContainerMacros.count[A]
  
  override def choose[B](q: PartialFunction[A, B]): Option[B] =
    macro ContainerMacros.choose[A, B]
}

private[general] object ContainerMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Container[A]] = {
    import c.universe._
    val Apply(_, container :: Nil) = c.prefix.tree
    val ContainerTag = c.weakTypeTag[Container[A]]
    c.Expr(c.typeCheck(container, ContainerTag.tpe))(ContainerTag)
  }
  
  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    while (!xs.isEmpty) {
      f.splice(xs.head)
      xs.step()
    }
  }
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    var r = z.splice
    while (!xs.isEmpty) {
      r = op.splice(r, xs.head)
      xs.step()
    }
    r
  }
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    if (xs.isEmpty) throw new UnsupportedOperationException
    else {
      var r: B = xs.head
      xs.step()
      while (!xs.isEmpty) {
        r = op.splice(r, xs.head)
        xs.step()
      }
      r
    }
  }
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Option[B]] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    if (xs.isEmpty) None
    else {
      var r: B = xs.head
      xs.step()
      while (!xs.isEmpty) {
        r = op.splice(r, xs.head)
        xs.step()
      }
      Some(r)
    }
  }
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Option[A]] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    var r: Option[A] = None
    while (r.isEmpty && !xs.isEmpty) {
      val x = xs.head
      if (p.splice(x)) r = Some(x)
      else xs.step()
    }
    r
  }
  
  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    var r = true
    while (r && !xs.isEmpty) {
      if (!p.splice(xs.head)) r = false
      else xs.step()
    }
    r
  }
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    var r = false
    while (!r && !xs.isEmpty) {
      if (p.splice(xs.head)) r = true
      else xs.step()
    }
    r
  }
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    var t = 0
    while (!xs.isEmpty) {
      if (p.splice(xs.head)) t += 1
      xs.step()
    }
    t
  }
  
  def choose[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Option[B]] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    var r: Option[B] = None
    while (r.isEmpty && !xs.isEmpty) {
      val x = xs.head
      if (q.splice.isDefinedAt(x)) r = Some(q.splice(x))
      else xs.step()
    }
    r
  }
}
