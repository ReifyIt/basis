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

/** Strictly evaluated container operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  container
  */
trait ContainerOps[+A, +From] extends Any with CollectionOps[A, From] with general.ContainerOps[A, From] {
  override def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[From, B, To]): To =
    macro ContainerMacros.collect[A, B, To]
  
  override def map[B, To](f: A => B)(implicit builder: Builder[From, B, To]): To =
    macro ContainerMacros.map[A, B, To]
  
  override def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[From, B, To]): To =
    macro ContainerMacros.flatMap[A, B, To]
  
  override def filter[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro ContainerMacros.filter[A, To]
  
  override def dropWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro ContainerMacros.dropWhile[A, To]
  
  override def takeWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro ContainerMacros.takeWhile[A, To]
  
  override def span[To](p: A => Boolean)(implicit builder1: Builder[From, A, To], builder2: Builder[From, A, To]): (To, To) =
    macro ContainerMacros.span[A, To]
  
  override def drop[To](lower: Int)(implicit builder: Builder[From, A, To]): To =
    macro ContainerMacros.drop[A, To]
  
  override def take[To](upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro ContainerMacros.take[A, To]
  
  override def slice[To](lower: Int, upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro ContainerMacros.slice[A, To]
  
  /** Returns pairs of elements from this and another container.
    * 
    * @param  those     the container whose elements to pair with these elements.
    * @param  builder   the accumulator for paired elements.
    * @return the accumulated pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B, To](those: Container[B])(implicit builder: Builder[From, (A, B), To]): To =
    macro ContainerMacros.zip[A, B, To]
}

private[strict] object ContainerMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Container[A]] = {
    import c.universe._
    val Apply(_, container :: Nil) = c.prefix.tree
    val ContainerTag = c.weakTypeTag[Container[A]]
    c.Expr(c.typeCheck(container, ContainerTag.tpe))(ContainerTag)
  }
  
  def collect[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val b = builder.splice
    while (!xs.isEmpty) {
      val x = xs.head
      if (q.splice.isDefinedAt(x)) b += q.splice(x)
      xs.step()
    }
    b.state
  }
  
  def map[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val b = builder.splice
    while (!xs.isEmpty) {
      b += f.splice(xs.head)
      xs.step()
    }
    b.state
  }
  
  def flatMap[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Enumerator[B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val b = builder.splice
    while (!xs.isEmpty) {
      b ++= f.splice(xs.head)
      xs.step()
    }
    b.state
  }
  
  def filter[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val b = builder.splice
    while (!xs.isEmpty) {
      val x = xs.head
      if (p.splice(x)) b += x
      xs.step()
    }
    b.state
  }
  
  def dropWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val b = builder.splice
    while (!xs.isEmpty && {
      val x = xs.head
      xs.step()
      p.splice(x) || { b += x; false }
    }) ()
    while (!xs.isEmpty) {
      b += xs.head
      xs.step()
    }
    b.state
  }
  
  def takeWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val b = builder.splice
    while (!xs.isEmpty && {
      val x = xs.head
      xs.step()
      p.splice(x) && { b += x; true }
    }) ()
    b.state
  }
  
  def span[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder1: c.Expr[Builder[_, A, To]], builder2: c.Expr[Builder[_, A, To]])
    : c.Expr[(To, To)] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val b1 = builder1.splice
    val b2 = builder2.splice
    while (!xs.isEmpty && {
      val x = xs.head
      xs.step()
      if (p.splice(x)) { b1 += x; true } else { b2 += x; false }
    })
    while (!xs.isEmpty) {
      b2 += xs.head
      xs.step()
    }
    (b1.state, b2.state)
  }
  
  def drop[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val n = lower.splice
    var i = 0
    val b = builder.splice
    while (i < n && !xs.isEmpty) {
      i += 1
      xs.step()
    }
    while (!xs.isEmpty) {
      b += xs.head
      xs.step()
    }
    b.state
  }
  
  def take[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val n = upper.splice
    var i = 0
    val b = builder.splice
    while (i < n && !xs.isEmpty) {
      b += xs.head
      i += 1
      xs.step()
    }
    b.state
  }
  
  def slice[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    var n = lower.splice
    var i = 0
    val b = builder.splice
    while (i < n && !xs.isEmpty) {
      i += 1
      xs.step()
    }
    n = upper.splice
    while (i < n && !xs.isEmpty) {
      b += xs.head
      i += 1
      xs.step()
    }
    b.state
  }
  
  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag, To: c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Container[B]])
      (builder: c.Expr[Builder[_, (A, B), To]])
    : c.Expr[To] = c.universe.reify {
    val xs = unApply[A](c).splice.iterator
    val ys = those.splice.iterator
    val b = builder.splice
    while (!xs.isEmpty && !ys.isEmpty) {
      b += ((xs.head, ys.head))
      xs.step()
      ys.step()
    }
    b.state
  }
}
