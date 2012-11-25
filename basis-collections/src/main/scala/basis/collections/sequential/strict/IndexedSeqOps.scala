/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential
package strict

import traversable._

/** Strictly evaluated indexed sequence operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  sequence
  */
trait IndexedSeqOps[+A, +From] extends Any with SeqOps[A, From] with general.IndexedSeqOps[A, From] {
  override def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[From, B, To]): To =
    macro IndexedSeqMacros.collect[A, B, To]
  
  override def map[B, To](f: A => B)(implicit builder: Builder[From, B, To]): To =
    macro IndexedSeqMacros.map[A, B, To]
  
  override def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[From, B, To]): To =
    macro IndexedSeqMacros.flatMap[A, B, To]
  
  override def filter[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro IndexedSeqMacros.filter[A, To]
  
  override def dropWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro IndexedSeqMacros.dropWhile[A, To]
  
  override def takeWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro IndexedSeqMacros.takeWhile[A, To]
  
  override def span[To](p: A => Boolean)(implicit builder1: Builder[From, A, To], builder2: Builder[From, A, To]): (To, To) =
    macro IndexedSeqMacros.span[A, To]
  
  override def drop[To](lower: Int)(implicit builder: Builder[From, A, To]): To =
    macro IndexedSeqMacros.drop[A, To]
  
  override def take[To](upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro IndexedSeqMacros.take[A, To]
  
  override def slice[To](lower: Int, upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro IndexedSeqMacros.slice[A, To]
  
  /** Returns the reverse of this $collection.
    * 
    * @param  builder   the accumulator for reversed elements.
    * @return the elements in this $collection in reverse order.
    * @group  Combining
    */
  def reverse[To](implicit builder: Builder[From, A, To]): To =
    macro IndexedSeqMacros.reverse[A, To]
}

private[strict] object IndexedSeqMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  import basis.util.IntOps
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[IndexedSeq[A]] = {
    import c.universe._
    val Apply(_, sequence :: Nil) = c.prefix.tree
    val IndexedSeqTag = c.weakTypeTag[IndexedSeq[A]]
    c.Expr(c.typeCheck(sequence, IndexedSeqTag.tpe))(IndexedSeqTag)
  }
  
  def collect[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    val b = builder.splice
    while (i < n) {
      val x = xs(i)
      if (q.splice.isDefinedAt(x)) b += q.splice(x)
      i += 1
    }
    b.state
  }
  
  def map[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    val b = builder.splice.expect(n)
    while (i < n) {
      b += f.splice(xs(i))
      i += 1
    }
    b.state
  }
  
  def flatMap[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Enumerator[B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    val b = builder.splice
    while (i < n) {
      b ++= f.splice(xs(i))
      i += 1
    }
    b.state
  }
  
  def filter[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    val b = builder.splice
    while (i < n) {
      val x = xs(i)
      if (p.splice(x)) b += x
      i += 1
    }
    b.state
  }
  
  def dropWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    val b = builder.splice
    while (i < n && {
      val x = xs(i)
      i += 1
      p.splice(x) || { b += x; false }
    }) ()
    while (i < n) {
      b += xs(i)
      i += 1
    }
    b.state
  }
  
  def takeWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    val b = builder.splice
    while (i < n && {
      val x = xs(i)
      i += 1
      p.splice(x) && { b += x; true }
    }) ()
    b.state
  }
  
  def span[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder1: c.Expr[Builder[_, A, To]], builder2: c.Expr[Builder[_, A, To]])
    : c.Expr[(To, To)] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0
    val b1 = builder1.splice
    val b2 = builder2.splice
    while (i < n && {
      val x = xs(i)
      i += 1
      if (p.splice(x)) { b1 += x; true } else { b2 += x; false }
    }) ()
    while (i < n) {
      b2 += xs(i)
      i += 1
    }
    (b1.state, b2.state)
  }
  
  def drop[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = xs.length
    var i = 0 max lower.splice min n
    val b = builder.splice.expect(n - i)
    while (i < n) {
      b += xs(i)
      i += 1
    }
    b.state
  }
  
  def take[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = upper.splice min xs.length
    var i = 0
    val b = builder.splice.expect(n)
    while (i < n) {
      b += xs(i)
      i += 1
    }
    b.state
  }
  
  def slice[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    val n = 0 max upper.splice min xs.length
    var i = 0 max lower.splice min n
    val b = builder.splice.expect(n - i)
    while (i < n) {
      b += xs(i)
      i += 1
    }
    b.state
  }
  
  def reverse[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice
    var i = xs.length
    val b = builder.splice.expect(i)
    i -= 1
    while (i >= 0) {
      b += xs(i)
      i -= 1
    }
    b.state
  }
}
