//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

private[sequential] abstract class ArrayLikeMacros(override val c: blackbox.Context) extends CollectionMacros(c) {
  import c.{ Expr, WeakTypeTag }
  import c.universe.{ Traverser => _, _ }

  def these: Expr[_]

  def foreach[A, U](f: Expr[A => U]): Expr[Unit] = Expr[Unit](q"""{
    val xs = $these
    var i = 0
    val n = xs.length
    while (i < n) {
      $f(xs(i))
      i += 1
    }
  }""")

  def foldLeft[A, B : WeakTypeTag](z: Expr[B])(op: Expr[(B, A) => B]): Expr[B] = Expr[B](q"""{
    val xs = $these
    var r = $z
    var i = 0
    val n = xs.length
    while (i < n) {
      r = $op(r, xs(i))
      i += 1
    }
    r
  }""")

  def reduceLeft[A, B >: A](op: Expr[(B, A) => B])(implicit B: WeakTypeTag[B]): Expr[B] = Expr[B](q"""{
    val xs = $these
    val n = xs.length
    if (n <= 0) throw new _root_.java.lang.UnsupportedOperationException("empty reduce")
    else {
      var r = xs(0): $B
      var i = 1
      while (i < n) {
        r = $op(r, xs(i))
        i += 1
      }
      r
    }
  }""")

  def mayReduceLeft[A, B >: A](op: Expr[(B, A) => B])(implicit B: WeakTypeTag[B]): Expr[Maybe[B]] = Expr[Maybe[B]](q"""{
    val xs = $these
    val n = xs.length
    if (n <= 0) _root_.basis.Trap
    else {
      var r = xs(0): $B
      var i = 1
      while (i < n) {
        r = $op(r, xs(i))
        i += 1
      }
      _root_.basis.Bind(r)
    }
  }""")

  def foldRight[A, B : WeakTypeTag](z: Expr[B])(op: Expr[(A, B) => B]): Expr[B] = Expr[B](q"""{
    val xs = $these
    var r = $z
    var i = xs.length - 1
    while (i >= 0) {
      r = $op(xs(i), r)
      i -= 1
    }
    r
  }""")

  def reduceRight[A, B >: A](op: Expr[(A, B) => B])(implicit B: WeakTypeTag[B]): Expr[B] = Expr[B](q"""{
    val xs = $these
    var i = xs.length - 1
    if (i < 0) throw new _root_.java.lang.UnsupportedOperationException("empty reduce")
    else {
      var r = xs(i): $B
      i -= 1
      while (i >= 0) {
        r = $op(xs(i), r)
        i -= 1
      }
      r
    }
  }""")

  def mayReduceRight[A, B >: A](op: Expr[(A, B) => B])(implicit B: WeakTypeTag[B]): Expr[Maybe[B]] = Expr[Maybe[B]](q"""{
    val xs = $these
    var i = xs.length - 1
    if (i < 0) _root_.basis.Trap
    else {
      var r = xs(i): $B
      i -= 1
      while (i >= 0) {
        r = $op(xs(i), r)
        i -= 1
      }
      _root_.basis.Bind(r)
    }
  }""")

  def find[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Maybe[A]] = {
    implicit val MaybeA = MaybeTag[A]
    Expr[Maybe[A]](q"""{
      val xs = $these
      var r = _root_.basis.Trap: $MaybeA
      var i = 0
      val n = xs.length
      while (i < n && {
        val x = xs(i)
        !$p(x) && { i += 1; true } || { r = _root_.basis.Bind(x); false }
      }) ()
      r
    }""")
  }

  def forall[A](p: Expr[A => Boolean]): Expr[Boolean] = Expr[Boolean](q"""{
    val xs = $these
    var r = true
    var i = 0
    val n = xs.length
    while (i < n && ($p(xs(i)) && { i += 1; true } || { r = false; false })) ()
    r
  }""")

  def exists[A](p: Expr[A => Boolean]): Expr[Boolean] = Expr[Boolean](q"""{
    val xs = $these
    var r = false
    var i = 0
    val n = xs.length
    while (i < n && (!$p(xs(i)) && { i += 1; true } || { r = true; false })) ()
    r
  }""")

  def count[A](p: Expr[A => Boolean]): Expr[Int] = Expr[Int](q"""{
    val xs = $these
    var t = 0
    var i = 0
    val n = xs.length
    while (i < n) {
      if ($p(xs(i))) t += 1
      i += 1
    }
    t
  }""")

  def choose[A, B : WeakTypeTag](q: Expr[PartialFunction[A, B]]): Expr[Maybe[B]] = {
    implicit val MaybeB = MaybeTag[B]
    Expr[Maybe[B]](q"""{
      val xs = $these
      var r = _root_.basis.Trap: $MaybeB
      var i = 0
      val n = xs.length
      val f = $q
      while (i < n && {
        val x = xs(i)
        f.isDefinedAt(x) && { r = _root_.basis.Bind(f.applyOrElse(x, _root_.scala.PartialFunction.empty)); false } || { i += 1; true }
      }) ()
      r
    }""")
  }

  def collect[A, B](q: Expr[PartialFunction[A, B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      var i = 0
      val n = xs.length
      val b = $builder: $builderType
      val f = $q
      while (i < n) {
        val x = xs(i)
        if (f.isDefinedAt(x)) b.append(f.applyOrElse(x, _root_.scala.PartialFunction.empty))
        i += 1
      }
      b.state
    }""")
  }

  def map[A, B](f: Expr[A => B])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      var i = 0
      val n = xs.length
      val b = $builder.expect(n): $builderType
      while (i < n) {
        b.append($f(xs(i)))
        i += 1
      }
      b.state
    }""")
  }

  def flatMap[A, B](f: Expr[A => Traverser[B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      var i = 0
      val n = xs.length
      val b = $builder: $builderType
      while (i < n) {
        b.appendAll($f(xs(i)))
        i += 1
      }
      b.state
    }""")
  }

  def filter[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      var i = 0
      val n = xs.length
      val b = $builder: $builderType
      while (i < n) {
        val x = xs(i)
        if ($p(x)) b.append(x)
        i += 1
      }
      b.state
    }""")
  }

  def dropWhile[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      var i = 0
      val n = xs.length
      val b = $builder: $builderType
      while (i < n && {
        val x = xs(i)
        i += 1
        $p(x) || { b.append(x); false }
      }) ()
      while (i < n) {
        b.append(xs(i))
        i += 1
      }
      b.state
    }""")
  }

  def takeWhile[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      var i = 0
      val n = xs.length
      val b = $builder: $builderType
      while (i < n && {
        val x = xs(i)
        $p(x) && { b.append(x); i += 1; true }
      }) ()
      b.state
    }""")
  }

  def span[A](p: Expr[A => Boolean])(builder1: Expr[Builder[A]], builder2: Expr[Builder[A]]): Expr[(builder1.value.State, builder2.value.State)] = {
    implicit val builder1Type = BuilderTypeTag(builder1)
    implicit val builder2Type = BuilderTypeTag(builder2)
    implicit val builder1State = BuilderStateTag(builder1)
    implicit val builder2State = BuilderStateTag(builder2)
    Expr[(builder1.value.State, builder2.value.State)](q"""{
      val xs = $these
      var i = 0
      val n = xs.length
      val b1 = $builder1: $builder1Type
      val b2 = $builder2: $builder2Type
      while (i < n && {
        val x = xs(i)
        i += 1
        if ($p(x)) { b1.append(x); true } else { b2.append(x); false }
      }) ()
      while (i < n) {
        b2.append(xs(i))
        i += 1
      }
      (b1.state: $builder1State, b2.state: $builder2State)
    }""")
  }

  def drop[A](lower: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      val n = xs.length
      var i = _root_.java.lang.Math.min(_root_.java.lang.Math.max(0, $lower), n)
      val b = $builder.expect(n - i): $builderType
      while (i < n) {
        b.append(xs(i))
        i += 1
      }
      b.state
    }""")
  }

  def take[A](upper: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      var i = 0
      val n = _root_.java.lang.Math.min(_root_.java.lang.Math.max(0, $upper), xs.length)
      val b = $builder.expect(n): $builderType
      while (i < n) {
        b.append(xs(i))
        i += 1
      }
      b.state
    }""")
  }

  def slice[A](lower: Expr[Int], upper: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      val n = _root_.java.lang.Math.min(_root_.java.lang.Math.max(0, $upper), xs.length)
      var i = _root_.java.lang.Math.min(_root_.java.lang.Math.max(0, $lower), n)
      val b = $builder.expect(n - i): $builderType
      while (i < n) {
        b.append(xs(i))
        i += 1
      }
      b.state
    }""")
  }

  def reverse[A](builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      var i = xs.length
      val b = $builder.expect(i): $builderType
      i -= 1
      while (i >= 0) {
        b.append(xs(i))
        i -= 1
      }
      b.state
    }""")
  }
}
