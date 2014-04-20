//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

private[sequential] abstract class LinearSeqMacros(override val c: blackbox.Context) extends TraverserMacros(c) {
  import c.{ Expr, WeakTypeTag }
  import c.universe.{ Traverser => _, _ }

  override def these: Expr[LinearSeq[_]]

  def foreach[A, U](f: Expr[A => U]): Expr[Unit] = Expr[Unit](q"""{
    var xs = $these
    while (!xs.isEmpty) {
      $f(xs.head)
      xs = xs.tail
    }
  }""")

  def foldLeft[A, B : WeakTypeTag](z: Expr[B])(op: Expr[(B, A) => B]): Expr[B] = Expr[B](q"""{
    var xs = $these
    var r = $z
    while (!xs.isEmpty) {
      r = $op(r, xs.head)
      xs = xs.tail
    }
    r
  }""")

  def reduceLeft[A, B >: A](op: Expr[(B, A) => B])(implicit B: WeakTypeTag[B]): Expr[B] = Expr[B](q"""{
    var xs = $these
    if (xs.isEmpty) throw new _root_.java.lang.UnsupportedOperationException("empty reduce")
    else {
      var r = xs.head: $B
      xs = xs.tail
      while (!xs.isEmpty) {
        r = $op(r, xs.head)
        xs = xs.tail
      }
      r
    }
  }""")

  def mayReduceLeft[A, B >: A](op: Expr[(B, A) => B])(implicit B: WeakTypeTag[B]): Expr[Maybe[B]] = Expr[Maybe[B]](q"""{
    var xs = $these
    if (xs.isEmpty) _root_.basis.util.Trap
    else {
      var r = xs.head: $B
      xs = xs.tail
      while (!xs.isEmpty) {
        r = $op(r, xs.head)
        xs = xs.tail
      }
      _root_.basis.util.Bind(r)
    }
  }""")

  def find[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Maybe[A]] = {
    implicit val MaybeA = MaybeTag[A]
    Expr[Maybe[A]](q"""{
      var xs = $these
      var r = _root_.basis.util.Trap: $MaybeA
      while (!xs.isEmpty && {
        val x = xs.head
        !$p(x) && { xs = xs.tail; true } || { r = _root_.basis.util.Bind(x); false }
      }) ()
      r
    }""")
  }

  def forall[A](p: Expr[A => Boolean]): Expr[Boolean] = Expr[Boolean](q"""{
    var xs = $these
    var r = true
    while (!xs.isEmpty && ($p(xs.head) && { xs = xs.tail; true } || { r = false; false })) ()
    r
  }""")

  def exists[A](p: Expr[A => Boolean]): Expr[Boolean] = Expr[Boolean](q"""{
    var xs = $these
    var r = false
    while (!xs.isEmpty && (!$p(xs.head) && { xs = xs.tail; true } || { r = true; false })) ()
    r
  }""")

  def count[A](p: Expr[A => Boolean]): Expr[Int] = Expr[Int](q"""{
    var xs = $these
    var t = 0
    while (!xs.isEmpty) {
      if ($p(xs.head)) t += 1
      xs = xs.tail
    }
    t
  }""")

  def choose[A, B : WeakTypeTag](q: Expr[PartialFunction[A, B]]): Expr[Maybe[B]] = {
    implicit val MaybeB = MaybeTag[B]
    Expr[Maybe[B]](q"""{
      var xs = $these
      var r = _root_.basis.util.Trap: $MaybeB
      val f = $q
      while (!xs.isEmpty && {
        val x = xs.head
        f.isDefinedAt(x) && { r = _root_.basis.util.Bind(f.applyOrElse(x, _root_.scala.PartialFunction.empty)); false } || { xs = xs.tail; true }
      }) ()
      r
    }""")
  }

  def collect[A, B](q: Expr[PartialFunction[A, B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      val b = $builder: $builderType
      val f = $q
      while (!xs.isEmpty) {
        val x = xs.head
        if (f.isDefinedAt(x)) b.append(f.applyOrElse(x, _root_.scala.PartialFunction.empty))
        xs = xs.tail
      }
      b.state
    }""")
  }

  def map[A, B](f: Expr[A => B])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      val b = $builder: $builderType
      while (!xs.isEmpty) {
        b.append($f(xs.head))
        xs = xs.tail
      }
      b.state
    }""")
  }

  def flatMap[A, B](f: Expr[A => Traverser[B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      val b = $builder: $builderType
      while (!xs.isEmpty) {
        b.appendAll($f(xs.head))
        xs = xs.tail
      }
      b.state
    }""")
  }

  def filter[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      val b = $builder: $builderType
      while (!xs.isEmpty) {
        val x = xs.head
        if ($p(x)) b.append(x)
        xs = xs.tail
      }
      b.state
    }""")
  }

  def dropWhile[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      val b = $builder: $builderType
      while (!xs.isEmpty && {
        val x = xs.head
        xs = xs.tail
        $p(x) || { b.append(x); false }
      }) ()
      while (!xs.isEmpty) {
        b.append(xs.head)
        xs = xs.tail
      }
      b.state
    }""")
  }

  def takeWhile[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      val b = $builder: $builderType
      while (!xs.isEmpty && {
        val x = xs.head
        $p(x) && { b.append(x); xs = xs.tail; true }
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
      var xs = $these
      val b1 = $builder1: $builder1Type
      val b2 = $builder2: $builder2Type
      while (!xs.isEmpty && {
        val x = xs.head
        xs = xs.tail
        if ($p(x)) { b1.append(x); true } else { b2.append(x); false }
      }) ()
      while (!xs.isEmpty) {
        b2.append(xs.head)
        xs = xs.tail
      }
      (b1.state: $builder1State, b2.state: $builder2State)
    }""")
  }

  def drop[A](lower: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      var i = 0
      val n = $lower
      val b = $builder: $builderType
      while (i < n && !xs.isEmpty) {
        xs = xs.tail
        i += 1
      }
      while (!xs.isEmpty) {
        b.append(xs.head)
        xs = xs.tail
      }
      b.state
    }""")
  }

  def take[A](upper: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      var i = 0
      val n = $upper
      val b = $builder: $builderType
      while (i < n && !xs.isEmpty) {
        b.append(xs.head)
        xs = xs.tail
        i += 1
      }
      b.state
    }""")
  }

  def slice[A](lower: Expr[Int], upper: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      var i = 0
      var n = $lower
      val b = $builder: $builderType
      while (i < n && !xs.isEmpty) {
        xs = xs.tail
        i += 1
      }
      n = $upper
      while (i < n && !xs.isEmpty) {
        b.append(xs.head)
        xs = xs.tail
        i += 1
      }
      b.state
    }""")
  }

  def zip[A, B](those: Expr[LinearSeq[B]])(builder: Expr[Builder[(A, B)]])(implicit A: WeakTypeTag[A], B: WeakTypeTag[B]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      var xs = $these
      var ys = $those
      val b = $builder: $builderType
      while (!xs.isEmpty && !ys.isEmpty) {
        b.append((xs.head: $A, ys.head: $B))
        xs = xs.tail
        ys = ys.tail
      }
      b.state
    }""")
  }
}
