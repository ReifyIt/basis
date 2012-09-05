/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

final class CharSeqOps[Kind](val __ : CharSeq) extends AnyVal {
  import __.{size, apply, advance}
  
  @inline def select[B](q: PartialFunction[Char, B]): Option[B] = {
    var i = 0
    val n = size
    while (i < n) {
      val x = apply(i)
      if (q.isDefinedAt(x)) return Some(q(x))
      i = advance(i)
    }
    None
  }
  
  
  
  @inline def foldLeft[B](z: B)(op: (B, Char) => B): B = {
    var result = z
    var i = 0
    val n = size
    while (i < n) {
      result = op(result, apply(i))
      i = advance(i)
    }
    result
  }
  
  @inline def reduceLeft[B >: Char](op: (B, Char) => B): B = {
    val n = size
    if (n == 0) throw new UnsupportedOperationException
    var result: B = apply(0)
    var i = advance(0)
    while (i < n) {
      result = op(result, apply(i))
      i = advance(i)
    }
    result
  }
  
  @inline def reduceLeftOption[B >: Char](op: (B, Char) => B): Option[B] = {
    val n = size
    if (n == 0) return None
    var result: B = apply(0)
    var i = advance(0)
    while (i < n) {
      result = op(result, apply(i))
      i = advance(i)
    }
    Some(result)
  }
  
  
  
  @inline def find(p: Char => Boolean): Option[Char] = {
    var i = 0
    val n = size
    while (i < n) {
      val x = apply(i)
      if (p(x)) return Some(x)
      i = advance(i)
    }
    None
  }
  
  @inline def forall(p: Char => Boolean): Boolean = {
    var i = 0
    val n = size
    while (i < n) {
      if (!p(apply(i))) return false
      i = advance(i)
    }
    true
  }
  
  @inline def exists(p: Char => Boolean): Boolean = {
    var i = 0
    val n = size
    while (i < n) {
      if (p(apply(i))) return true
      i = advance(i)
    }
    false
  }
  
  @inline def count(p: Char => Boolean): Int = {
    var total = 0
    var i = 0
    val n = size
    while (i < n) {
      if (p(apply(i))) total += 1
      i = advance(i)
    }
    total
  }
  
  @inline def map[B](f: Char => B)(implicit builder: Builder[Kind, B]): builder.Result = {
    var i = 0
    val n = size
    while (i < n) {
      builder += f(apply(i))
      i = advance(i)
    }
    builder.result
  }
  
  
  
  @inline def filter(p: Char => Boolean)(implicit builder: Builder[Kind, Char]): builder.Result = {
    var i = 0
    val n = size
    while (i < n) {
      val x = apply(i)
      if (p(x)) builder += x
      i = advance(i)
    }
    builder.result
  }
  
  @inline def collect[B](q: PartialFunction[Char, B])(implicit builder: Builder[Kind, B]): builder.Result = {
    var i = 0
    val n = size
    while (i < n) {
      val x = apply(i)
      if (q.isDefinedAt(x)) builder += q(x)
      i = advance(i)
    }
    builder.result
  }
  
  @inline def dropWhile(p: Char => Boolean)(implicit builder: Builder[Kind, Char]): builder.Result = {
    var i = 0
    var x = new Char(0)
    val n = size
    while (i < n && { x = apply(i); p(x) }) i = advance(i)
    if (i < n) { builder += x; i = advance(i) }
    while (i < n) { builder += apply(i); i = advance(i) }
    builder.result
  }
  
  @inline def takeWhile(p: Char => Boolean)(implicit builder: Builder[Kind, Char]): builder.Result = {
    var i = 0
    var x = new Char(0)
    val n = size
    while (i < n && { x = apply(i); p(x) }) { builder += x; i = advance(i) }
    builder.result
  }
  
  @inline def span(p: Char => Boolean)(
      implicit builderA: Builder[Kind, Char],
               builderB: Builder[Kind, Char]): (builderA.Result, builderB.Result) = {
    var i = 0
    var x = new Char(0)
    val n = size
    while (i < n && { x = apply(i); p(x) }) { builderA += x; i = advance(i) }
    if (i < n) { builderB += x; i = advance(i) }
    while (i < n) { builderB += apply(i); i = advance(i) }
    (builderA.result, builderB.result)
  }
  
  def drop(lower: Int)(implicit builder: Builder[Kind, Char]): builder.Result = {
    var c = 0
    var i = 0
    val n = size
    while (c < lower && i < n) {
      c += 1
      i = advance(i)
    }
    while (i < n) {
      builder += apply(i)
      i = advance(i)
    }
    builder.result
  }
  
  def take(upper: Int)(implicit builder: Builder[Kind, Char]): builder.Result = {
    var c = 0
    var i = 0
    val n = size
    while (c < upper && i < n) {
      builder += apply(i)
      c += 1
      i = advance(i)
    }
    builder.result
  }
  
  def slice(lower: Int, upper: Int)(implicit builder: Builder[Kind, Char]): builder.Result = {
    var c = 0
    var i = 0
    val n = size
    while (c < lower && i < n) {
      c += 1
      i = advance(i)
    }
    while (c < upper && i < n) {
      builder += apply(i)
      c += 1
      i = advance(i)
    }
    builder.result
  }
  
  def ++ (that: Text)(implicit builder: Builder[Kind, Char]): builder.Result = {
    var i = 0
    var n = size
    while (i < n) {
      builder += apply(i)
      i = advance(i)
    }
    i = 0
    n = that.size
    while (i < n) {
      builder += that.apply(i)
      i = that.advance(i)
    }
    builder.result
  }
}
