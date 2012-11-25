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

/** Strictly evaluated linear sequence operations.
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
trait LinearSeqOps[+A, +From] extends Any with SeqOps[A, From] with general.LinearSeqOps[A, From] {
  override def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[From, B, To]): To =
    macro LinearSeqMacros.collect[A, B, To]
  
  override def map[B, To](f: A => B)(implicit builder: Builder[From, B, To]): To =
    macro LinearSeqMacros.map[A, B, To]
  
  override def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[From, B, To]): To =
    macro LinearSeqMacros.flatMap[A, B, To]
  
  override def filter[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro LinearSeqMacros.filter[A, To]
  
  override def dropWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro LinearSeqMacros.dropWhile[A, To]
  
  override def takeWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro LinearSeqMacros.takeWhile[A, To]
  
  override def span[To](p: A => Boolean)(implicit builder1: Builder[From, A, To], builder2: Builder[From, A, To]): (To, To) =
    macro LinearSeqMacros.span[A, To]
  
  override def drop[To](lower: Int)(implicit builder: Builder[From, A, To]): To =
    macro LinearSeqMacros.drop[A, To]
  
  override def take[To](upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro LinearSeqMacros.take[A, To]
  
  override def slice[To](lower: Int, upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro LinearSeqMacros.slice[A, To]
}

private[strict] object LinearSeqMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[LinearSeq[A]] = {
    import c.universe._
    val Apply(_, sequence :: Nil) = c.prefix.tree
    val LinearSeqTag = c.weakTypeTag[LinearSeq[A]]
    c.Expr(c.typeCheck(sequence, LinearSeqTag.tpe))(LinearSeqTag)
  }
  
  def collect[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    val b = builder.splice
    while (!xs.isEmpty) {
      val x = xs.head
      if (q.splice.isDefinedAt(x)) b += q.splice(x)
      xs = xs.tail
    }
    b.state
  }
  
  def map[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    val b = builder.splice
    while (!xs.isEmpty) {
      b += f.splice(xs.head)
      xs = xs.tail
    }
    b.state
  }
  
  def flatMap[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Enumerator[B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    val b = builder.splice
    while (!xs.isEmpty) {
      b ++= f.splice(xs.head)
      xs = xs.tail
    }
    b.state
  }
  
  def filter[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    val b = builder.splice
    while (!xs.isEmpty) {
      val x = xs.head
      if (p.splice(x)) b += x
      xs = xs.tail
    }
    b.state
  }
  
  def dropWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    val b = builder.splice
    while (!xs.isEmpty && {
      val x = xs.head
      xs = xs.tail
      p.splice(x) || { b += x; false }
    }) ()
    while (!xs.isEmpty) {
      b += xs.head
      xs = xs.tail
    }
    b.state
  }
  
  def takeWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    val b = builder.splice
    while (!xs.isEmpty && {
      val x = xs.head
      xs = xs.tail
      p.splice(x) && { b += x; true }
    }) ()
    b.state
  }
  
  def span[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder1: c.Expr[Builder[_, A, To]], builder2: c.Expr[Builder[_, A, To]])
    : c.Expr[(To, To)] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    val b1 = builder1.splice
    val b2 = builder2.splice
    while (!xs.isEmpty && {
      val x = xs.head
      xs = xs.tail
      if (p.splice(x)) { b1 += x; true } else { b2 += x; false }
    })
    while (!xs.isEmpty) {
      b2 += xs.head
      xs = xs.tail
    }
    (b1.state, b2.state)
  }
  
  def drop[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    val n = lower.splice
    var i = 0
    val b = builder.splice
    while (i < n && !xs.isEmpty) {
      i += 1
      xs = xs.tail
    }
    while (!xs.isEmpty) {
      b += xs.head
      xs = xs.tail
    }
    b.state
  }
  
  def take[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    val n = upper.splice
    var i = 0
    val b = builder.splice
    while (i < n && !xs.isEmpty) {
      b += xs.head
      i += 1
      xs = xs.tail
    }
    b.state
  }
  
  def slice[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    var xs: LinearSeq[A] = unApply[A](c).splice
    var n = lower.splice
    var i = 0
    val b = builder.splice
    while (i < n && !xs.isEmpty) {
      i += 1
      xs = xs.tail
    }
    n = upper.splice
    while (i < n && !xs.isEmpty) {
      b += xs.head
      i += 1
      xs = xs.tail
    }
    b.state
  }
}
