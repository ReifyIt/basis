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

/** General indexed sequence operations.
  * 
  * @groupprio  Traversing  -3
  * @groupprio  Reducing    -2
  * @groupprio  Querying    -1
  * 
  * @define collection  sequence
  */
trait IndexedSeqOps[+A, +From] extends Any with SeqOps[A, From] {
  override def foreach[U](f: A => U): Unit =
    macro IndexedSeqMacros.foreach[A, U]
  
  override def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro IndexedSeqMacros.foldLeft[A, B]
  
  override def reduce[B >: A](op: (B, B) => B): B =
    macro IndexedSeqMacros.reduceLeft[A, B]
  
  override def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro IndexedSeqMacros.reduceLeftOption[A, B]
  
  override def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro IndexedSeqMacros.foldLeft[A, B]
  
  override def reduceLeft[B >: A](op: (B, A) => B): B =
    macro IndexedSeqMacros.reduceLeft[A, B]
  
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro IndexedSeqMacros.reduceLeftOption[A, B]
  
  /** Returns the right-to-left application of a binary operator between a
    * start value and all elements in this $collection.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply left-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldRight[B](z: B)(op: (A, B) => B): B =
    macro IndexedSeqMacros.foldRight[A, B]
  
  /** Returns the right-to-left application of a binary operator between
    * all elements in this non-empty $collection.
    * 
    * @param  op  the binary operator to apply left-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceRight[B >: A](op: (A, B) => B): B =
    macro IndexedSeqMacros.reduceRight[A, B]
  
  /** Returns the right-to-left application of a binary operator between
    * all elements in this $collection.
    * 
    * @param  op  the binary operator to apply left-recursively.
    * @return some reduced value, or none if this $collection is empty.
    * @group  Reducing
    */
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] =
    macro IndexedSeqMacros.reduceRightOption[A, B]
  
  override def find(p: A => Boolean): Option[A] =
    macro IndexedSeqMacros.find[A]
  
  override def forall(p: A => Boolean): Boolean =
    macro IndexedSeqMacros.forall[A]
  
  override def exists(p: A => Boolean): Boolean =
    macro IndexedSeqMacros.exists[A]
  
  override def count(p: A => Boolean): Int =
    macro IndexedSeqMacros.count[A]
  
  override def choose[B](q: PartialFunction[A, B]): Option[B] =
    macro IndexedSeqMacros.choose[A, B]
}

private[general] object IndexedSeqMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[IndexedSeq[A]] = {
    import c.universe._
    val Apply(_, sequence :: Nil) = c.prefix.tree
    val IndexedSeqTag = c.weakTypeTag[IndexedSeq[A]]
    c.Expr(c.typeCheck(sequence, IndexedSeqTag.tpe))(IndexedSeqTag)
  }
  
  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    while (i < n) {
      f.splice(xs(i))
      i += 1
    }
  }
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    var r = z.splice
    while (i < n) {
      r = op.splice(r, xs(i))
      i += 1
    }
    r
  }
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    if (n <= 0) throw new UnsupportedOperationException
    else {
      var i = 1
      var r: B = xs(0)
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
    : c.Expr[Option[B]] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    if (n <= 0) None
    else {
      var i = 1
      var r: B = xs(0)
      while (i < n) {
        r = op.splice(r, xs(i))
        i += 1
      }
      Some(r)
    }
  }
  
  def foldRight[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(A, B) => B])
    : c.Expr[B] = c.universe.reify {
    val xs = unApply[A](c).splice
    var i = xs.length - 1
    var r = z.splice
    while (i >= 0) {
      r = op.splice(xs(i), r)
      i -= 1
    }
    r
  }
  
  def reduceRight[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(A, B) => B])
    : c.Expr[B] = c.universe.reify {
    val xs = unApply[A](c).splice
    var i = xs.length - 1
    if (i < 0) throw new UnsupportedOperationException
    else {
      var r: B = xs(i)
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
    : c.Expr[Option[B]] = c.universe.reify {
    val xs = unApply[A](c).splice
    var i = xs.length - 1
    if (i < 0) None
    else {
      var r: B = xs(i)
      i -= 1
      while (i >= 0) {
        r = op.splice(xs(i), r)
        i -= 1
      }
      Some(r)
    }
  }
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Option[A]] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    var r: Option[A] = None
    while (r.isEmpty && i < n) {
      val x = xs(i)
      if (p.splice(x)) r = Some(x)
      else i += 1
    }
    r
  }
  
  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    var r = true
    while (r && i < n) {
      if (!p.splice(xs(i))) r = false
      else i += 1
    }
    r
  }
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    var r = false
    while (!r && i < n) {
      if (p.splice(xs(i))) r = true
      else i += 1
    }
    r
  }
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    var t = 0
    while (i < n) {
      if (p.splice(xs(i))) t += 1
      i += 1
    }
    t
  }
  
  def choose[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Option[B]] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    var r: Option[B] = None
    while (r.isEmpty && i < n) {
      val x = xs(i)
      if (q.splice.isDefinedAt(x)) r = Some(q.splice(x))
      else i += 1
    }
    r
  }
}
