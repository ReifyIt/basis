/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

final class IterableOps[Kind, A](val __ : Iterable[A]) extends AnyVal {
  import __.iterator
  
  @inline def select[B](q: PartialFunction[A, B]): Option[B] = {
    val iter = iterator
    while (iter.hasNext) {
      val x = iter.next()
      if (q.isDefinedAt(x)) return Some(q(x))
    }
    None
  }
  
  @inline def fold[B >: A](z: B)(op: (B, B) => B): B = {
    val iter = iterator
    var result = z
    while (iter.hasNext) result = op(result, iter.next())
    result
  }
  
  @inline def reduce[B >: A](op: (B, B) => B): B = {
    val iter = iterator
    if (!iter.hasNext) throw new UnsupportedOperationException
    var result: B = iter.next()
    while (iter.hasNext) result = op(result, iter.next())
    result
  }
  
  @inline def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
    val iter = iterator
    if (iter.hasNext) return None
    var result: B = iter.next()
    while (iter.hasNext) result = op(result, iter.next())
    Some(result)
  }
  
  @inline def find(p: A => Boolean): Option[A] = {
    val iter = iterator
    while (iter.hasNext) { val x = iter.next(); if (p(x)) return Some(x) }
    None
  }
  
  @inline def forall(p: A => Boolean): Boolean = {
    val iter = iterator
    while (iter.hasNext) if (!p(iter.next())) return false
    true
  }
  
  @inline def exists(p: A => Boolean): Boolean = {
    val iter = iterator
    while (iter.hasNext) if (p(iter.next())) return true
    false
  }
  
  @inline def count(p: A => Boolean): Int = {
    val iter = iterator
    var total = 0
    while (iter.hasNext) if (p(iter.next())) total += 1
    total
  }
  
  @inline def map[B](f: A => B)(implicit builder: Builder[Kind, B]): builder.Result = {
    val iter = iterator
    while (iter.hasNext) builder += f(iter.next())
    builder.result
  }
  
  @inline def flatMap[B](f: A => Iterable[B])(implicit builder: Builder[Kind, B]): builder.Result = {
    val these = iterator
    while (these.hasNext) {
      val those = f(these.next()).iterator
      while (those.hasNext) builder += those.next()
    }
    builder.result
  }
  
  @inline def filter(p: A => Boolean)(implicit builder: Builder[Kind, A]): builder.Result = {
    val iter = iterator
    while (iter.hasNext) { val x = iter.next(); if (p(x)) builder += x }
    builder.result
  }
  
  def withFilter(p: A => Boolean): Iterable[A] = new IterableOps.WithFilter(__, p)
  
  @inline def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[Kind, B]): builder.Result = {
    val iter = iterator
    while (iter.hasNext) { val x = iter.next(); if (q.isDefinedAt(x)) builder += q(x) }
    builder.result
  }
  
  def zip[B](other: Iterable[B])(implicit builder: Builder[Kind, (A, B)]): builder.Result = {
    val these = iterator
    val those = other.iterator
    while (these.hasNext && those.hasNext) builder += ((these.next(), those.next()))
    builder.result
  }
}

private[collection] object IterableOps {
  final class WithFilter[+A](self: Iterable[A], p: A => Boolean) extends Iterable[A] {
    override def iterator: Iterator[A] = self.iterator filter p
    override def foreach[U](f: A => U): Unit = self.foreach(new TraversableOps.Filter(f, p))
  }
}
