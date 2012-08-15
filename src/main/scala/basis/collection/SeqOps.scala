/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

final class SeqOps[Kind, A](val __ : Seq[A]) extends AnyVal {
  import __.iterator
  
  @inline def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val iter = iterator
    var result = z
    while (iter.hasNext) result = op(result, iter.next())
    result
  }
  
  @inline def reduceLeft[B >: A](op: (B, A) => B): B = {
    val iter = iterator
    if (!iter.hasNext) throw new UnsupportedOperationException
    var result: B = iter.next()
    while (iter.hasNext) result = op(result, iter.next())
    result
  }
  
  @inline def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    val iter = iterator
    if (!iter.hasNext) return None
    var result: B = iter.next()
    while (iter.hasNext) result = op(result, iter.next())
    Some(result)
  }
  
  def withFilter(p: A => Boolean): Seq[A] = new SeqOps.WithFilter(__, p)
  
  @inline def dropWhile(p: A => Boolean)(implicit builder: Builder[Kind, A]): builder.Result = {
    val iter = iterator
    while (iter.hasNext) {
      val x = iter.next()
      if (!p(x)) {
        builder += x
        while (iter.hasNext) builder += iter.next()
        return builder.result
      }
    }
    builder.result
  }
  
  @inline def takeWhile(p: A => Boolean)(implicit builder: Builder[Kind, A]): builder.Result = {
    val iter = iterator
    while (iter.hasNext) {
      val x = iter.next()
      if (p(x)) builder += x
      else return builder.result
    }
    builder.result
  }
  
  @inline def span(p: A => Boolean)(
      implicit builderA: Builder[Kind, A],
               builderB: Builder[Kind, A]): (builderA.Result, builderB.Result) = {
    val iter = iterator
    while (iter.hasNext) {
      val x = iter.next()
      if (p(x)) builderA += x
      else {
        builderB += x
        while (iter.hasNext) builderB += iter.next()
        return (builderA.result, builderB.result)
      }
    }
    (builderA.result, builderB.result)
  }
  
  def drop(lower: Int)(implicit builder: Builder[Kind, A]): builder.Result = {
    val iter = iterator
    var i = 0
    while (i < lower && iter.hasNext) { iter.next(); i += 1 }
    while (iter.hasNext) builder += iter.next()
    builder.result
  }
  
  def take(upper: Int)(implicit builder: Builder[Kind, A]): builder.Result = {
    val iter = iterator
    var i = 0
    while (i < upper && iter.hasNext) { builder += iter.next(); i += 1 }
    builder.result
  }
  
  def slice(lower: Int, upper: Int)(implicit builder: Builder[Kind, A]): builder.Result = {
    val iter = iterator
    if (lower < upper) {
      var i = 0
      while (i < lower && iter.hasNext) { iter.next(); i += 1 }
      while (i < upper && iter.hasNext) { builder += iter.next(); i += 1 }
    }
    builder.result
  }
}

private[collection] object SeqOps {
  final class WithFilter[+A](self: Seq[A], p: A => Boolean) extends Seq[A] {
    override def iterator: Iterator[A] = self.iterator filter p
    override def foreach[U](f: A => U): Unit = self.foreach(new TraversableOps.Filter(f, p))
  }
}
