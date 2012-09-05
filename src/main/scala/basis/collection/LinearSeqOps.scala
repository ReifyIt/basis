/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

final class LinearSeqOps[Kind, A](val __ : LinearSeq[A]) extends AnyVal {
  @inline def select[B](q: PartialFunction[A, B]): Option[B] = {
    var these = __
    while (!these.isEmpty) {
      val x = these.head
      if (q.isDefinedAt(x)) return Some(q(x))
      these = these.tail
    }
    None
  }
  
  @inline def fold[B >: A](z: B)(op: (B, B) => B): B = {
    var these = __
    var result = z
    while (!these.isEmpty) {
      result = op(result, these.head)
      these = these.tail
    }
    result
  }
  
  @inline def reduce[B >: A](op: (B, B) => B): B = {
    var these = __
    if (these.isEmpty) throw new UnsupportedOperationException
    var result: B = these.head
    these = these.tail
    while (!these.isEmpty) {
      result = op(result, these.head)
      these = these.tail
    }
    result
  }
  
  @inline def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
    var these = __
    if (these.isEmpty) return None
    var result: B = these.head
    these = these.tail
    while (!these.isEmpty) {
      result = op(result, these.head)
      these = these.tail
    }
    Some(result)
  }
  
  @inline def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var these = __
    var result = z
    while (!these.isEmpty) {
      result = op(result, these.head)
      these = these.tail
    }
    result
  }
  
  @inline def reduceLeft[B >: A](op: (B, A) => B): B = {
    var these = __
    if (these.isEmpty) throw new UnsupportedOperationException
    var result: B = these.head
    these = these.tail
    while (!these.isEmpty) {
      result = op(result, these.head)
      these = these.tail
    }
    result
  }
  
  @inline def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    var these = __
    if (these.isEmpty) return None
    var result: B = these.head
    these = these.tail
    while (!these.isEmpty) {
      result = op(result, these.head)
      these = these.tail
    }
    Some(result)
  }
  
  @inline def find(p: A => Boolean): Option[A] = {
    var these = __
    while (!these.isEmpty) {
      val x = these.head
      if (p(x)) return Some(x)
      these = these.tail
    }
    None
  }
  
  @inline def forall(p: A => Boolean): Boolean = {
    var these = __
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }
  
  @inline def exists(p: A => Boolean): Boolean = {
    var these = __
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }
  
  @inline def count(p: A => Boolean): Int = {
    var these = __
    var total = 0
    while (!these.isEmpty) {
      if (p(these.head)) total += 1
      these = these.tail
    }
    total
  }
  
  @inline def map[B](f: A => B)(implicit builder: Builder[Kind, B]): builder.Result = {
    var these = __
    while (!these.isEmpty) {
      builder += f(these.head)
      these = these.tail
    }
    builder.result
  }
  
  @inline def flatMap[B](f: A => LinearSeq[B])(implicit builder: Builder[Kind, B]): builder.Result = {
    var these = __
    while (!these.isEmpty) {
      var those = f(these.head)
      while (!those.isEmpty) {
        builder += those.head
        those = those.tail
      }
      these = these.tail
    }
    builder.result
  }
  
  @inline def filter(p: A => Boolean)(implicit builder: Builder[Kind, A]): builder.Result = {
    var these = __
    while (!these.isEmpty) {
      val x = these.head
      if (p(x)) builder += x
      these = these.tail
    }
    builder.result
  }
  
  @inline def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[Kind, B]): builder.Result = {
    var these = __
    while (!these.isEmpty) {
      val x = these.head
      if (q.isDefinedAt(x)) builder += q(x)
      these = these.tail
    }
    builder.result
  }
  
  @inline def dropWhile(p: A => Boolean)(implicit builder: Builder[Kind, A]): builder.Result = {
    var these = __
    var x = null.asInstanceOf[A]
    while (!these.isEmpty && { x = these.head; p(x) }) these = these.tail
    if (!these.isEmpty) { builder += x; these = these.tail }
    while (!these.isEmpty) { builder += these.head; these = these.tail }
    builder.result
  }
  
  @inline def takeWhile(p: A => Boolean)(implicit builder: Builder[Kind, A]): builder.Result = {
    var these = __
    var x = null.asInstanceOf[A]
    while (!these.isEmpty && { x = these.head; p(x) }) { builder += x; these = these.tail }
    builder.result
  }
  
  @inline def span(p: A => Boolean)(
      implicit builderA: Builder[Kind, A],
               builderB: Builder[Kind, A]): (builderA.Result, builderB.Result) = {
    var these = __
    var x = null.asInstanceOf[A]
    while (!these.isEmpty && { x = these.head; p(x) }) { builderA += x; these = these.tail }
    if (!these.isEmpty) { builderB += x; these = these.tail }
    while (!these.isEmpty) { builderB += these.head; these = these.tail }
    (builderA.result, builderB.result)
  }
  
  def drop(lower: Int)(implicit builder: Builder[Kind, A]): builder.Result = {
    var these = __
    var i = 0
    while (!these.isEmpty && i < lower) { these = these.tail; i += 1 }
    while (!these.isEmpty) { builder += these.head; these = these.tail }
    builder.result
  }
  
  def take(upper: Int)(implicit builder: Builder[Kind, A]): builder.Result = {
    var these = __
    var i = 0
    while (!these.isEmpty && i < upper) { builder += these.head; these = these.tail; i += 1 }
    builder.result
  }
  
  def slice(lower: Int, upper: Int)(implicit builder: Builder[Kind, A]): builder.Result = {
    var these = __
    var i = 0
    while (!these.isEmpty && i < lower) { these = these.tail; i += 1 }
    while (!these.isEmpty && i < upper) { builder += these.head; these = these.tail; i += 1 }
    builder.result
  }
  
  def ++ [B >: A](that: LinearSeq[B])(implicit builder: Builder[Kind, B]): builder.Result = {
    var these = __
    while (!these.isEmpty) {
      builder += these.head
      these = these.tail
    }
    these = that
    while (!these.isEmpty) {
      builder += these.head
      these = these.tail
    }
    builder.result
  }
}
