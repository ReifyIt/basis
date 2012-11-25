/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import traversable._

package object general {
  implicit def EnumeratorOps[A](self: Enumerator[A]): EnumeratorOps[A, self.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def IteratorOps[A](self: Iterator[A]): IteratorOps[A, self.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def CollectionOps[A](self: Collection[A]): CollectionOps[A, self.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def ContainerOps[A](self: Container[A]): ContainerOps[A, self.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def SeqOps[A](self: Seq[A]): SeqOps[A, self.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def LinearSeqOps[A](self: LinearSeq[A]): LinearSeqOps[A, self.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def IndexedSeqOps[A](self: IndexedSeq[A]): IndexedSeqOps[A, self.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def SetOps[A](self: Set[A]): SetOps[A, self.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit def MapOps[A, T](self: Map[A, T]): MapOps[A, T, self.Parent] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  def foldLeft[A, B](xs: Enumerator[A])(z: B)(op: (B, A) => B): B = {
    var r = z
    traverse(xs)(x => r = op(r, x))
    r
  }
  
  def reduceLeft[A, B >: A](xs: Enumerator[A])(op: (B, A) => B): B = {
    var r = null.asInstanceOf[B]
    var e = true
    traverse(xs)(x => if (e) { r = x; e = false } else r = op(r, x))
    if (e) throw new UnsupportedOperationException("Empty reduce.")
    else r
  }
  
  def reduceLeftOption[A, B >: A](xs: Enumerator[A])(op: (B, A) => B): Option[B] = {
    var r = null.asInstanceOf[B]
    var e = true
    traverse(xs)(x => if (e) { r = x; e = false } else r = op(r, x))
    if (e) None
    else Some(r)
  }
  
  def find[A](xs: Enumerator[A])(p: A => Boolean): Option[A] = {
    var r: Option[A] = None
    label(traverse(xs)(x => if (p(x)) { r = Some(x); label.break() }))
    r
  }
  
  def forall[A](xs: Enumerator[A])(p: A => Boolean): Boolean = {
    var r = true
    label(traverse(xs)(x => if (!p(x)) { r = false; label.break() }))
    r
  }
  
  def exists[A](xs: Enumerator[A])(p: A => Boolean): Boolean = {
    var r = false
    label(traverse(xs)(x => if (p(x)) { r = true; label.break() }))
    r
  }
  
  def count[A](xs: Enumerator[A])(p: A => Boolean): Int = {
    var t = 0
    traverse(xs)(x => if (p(x)) t += 1)
    t
  }
  
  def choose[A, B](xs: Enumerator[A])(q: PartialFunction[A, B]): Option[B] = {
    var r: Option[B] = None
    label(traverse(xs)(x => if (q.isDefinedAt(x)) { r = Some(q(x)); label.break() }))
    r
  }
}
