/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import traversable._

package object strict {
  import basis.util.IntOps
  
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
  
  def collect[A, B, To](xs: Enumerator[A])(q: PartialFunction[A, B])(implicit builder: Builder[Nothing, B, To]): To = {
    traverse(xs)(x => if (q.isDefinedAt(x)) builder += q(x))
    builder.state
  }
  
  def map[A, B, To](xs: Enumerator[A])(f: A => B)(implicit builder: Builder[Nothing, B, To]): To = {
    traverse(xs)(x => builder += f(x))
    builder.state
  }
  
  def flatMap[A, B, To](xs: Enumerator[A])(f: A => Enumerator[B])(implicit builder: Builder[Nothing, B, To]): To = {
    traverse(xs)(x => builder ++= f(x))
    builder.state
  }
  
  def filter[A, To](xs: Enumerator[A])(p: A => Boolean)(implicit builder: Builder[Nothing, A, To]): To = {
    traverse(xs)(x => if (p(x)) builder += x)
    builder.state
  }
  
  def dropWhile[A, To](xs: Enumerator[A])(p: A => Boolean)(implicit builder: Builder[Nothing, A, To]): To = {
    var split = false
    traverse(xs)(x => if (split || (!p(x) && { split = true; true })) builder += x)
    builder.state
  }
  
  def takeWhile[A, To](xs: Enumerator[A])(p: A => Boolean)(implicit builder: Builder[Nothing, A, To]): To = {
    label(traverse(xs)(x => if (p(x)) builder += x else label.break()))
    builder.state
  }
  
  def span[A, To](xs: Enumerator[A])(p: A => Boolean)(implicit builder1: Builder[Nothing, A, To], builder2: Builder[Nothing, A, To]): (To, To) = {
    var split = false
    traverse(xs)(x => (if (!split && (p(x) || { split = true; false })) builder1 else builder2) += x)
    (builder1.state, builder2.state)
  }
  
  def drop[A, To](xs: Enumerator[A])(lower: Int)(implicit builder: Builder[Nothing, A, To]): To = {
    var i = 0
    traverse(xs)(x => if (i >= lower) builder += x else i += 1)
    builder.state
  }
  
  def take[A, To](xs: Enumerator[A])(upper: Int)(implicit builder: Builder[Nothing, A, To]): To = {
    var i = 0
    label(traverse(xs)(x => if (i < upper) { builder += x; i += 1 } else label.break()))
    builder.state
  }
  
  def slice[A, To](xs: Enumerator[A])(lower: Int, upper: Int)(implicit builder: Builder[Nothing, A, To]): To = {
    var l = 0 max lower
    var u = l max upper
    var i = 0
    if (l < u) label(traverse(xs)(x => if (i < u) { if (i >= l) builder += x; i += 1 } else label.break()))
    builder.state
  }
}
