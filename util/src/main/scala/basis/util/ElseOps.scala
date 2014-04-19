//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.Predef.<:<
import scala.reflect.macros._

final class ElseOps[+A, +B](val __ : A Else B) extends AnyVal {
  /** Returns the value of this `Bind`, or the `default` value for a `Trap`.
    * @group Evaluating */
  def bindOrElse[X >: A](default: => X): X = macro ElseMacros.bindOrElse[X, B]

  /** Returns this `Bind`, or the `other` binding for a `Trap`.
    * @group Composing */
  def orElse[X >: A, Y](other: X Else Y): X Else Y = macro ElseMacros.orElse[X, Y]

  /** Returns the value of this `Bind`, or `null` for a `Trap`.
    * @group Evaluating */
  def orNull[X >: A](implicit isNullable: Null <:< X): X = macro ElseMacros.orNull[X]

  /** Apllies a function to the value of this `Bind`, or returns the zero
    * value for a `Trap`; equivalent to `map f bindOrElse z`.
    * @group Evaluating */
  def fold[X](z: X)(f: A => X): X = macro ElseMacros.fold[A, X]

  /** Returns `true` if the value of this `Bind` satisfies the predicate,
    * otherwise returns `false`.
    * @group Evaluating */
  def exists(p: A => Boolean): Boolean = macro ElseMacros.exists[A]

  /** Applies a function to the value of this `Bind`, otherwise does nothing.
    * @group Evaluating */
  def foreach[U](f: A => U): Unit = macro ElseMacros.foreach[A, U]

  /** Binds the result of a function applied to the value of this `Bind`,
    * otherwise returns this `Trap`.
    * @group Composing */
  def map[X](f: A => X): X Else B = macro ElseMacros.map[A, X, B]

  /** Returns the binding of a function applied to the value of this `Bind`,
    * otherwise returns this `Trap`.
    * @group Composing */
  def flatMap[X, Y >: B](f: A => (X Else Y)): X Else Y = macro ElseMacros.flatMap[A, X, Y]

  /** Binds the result of a function applied to the value of this `Trap`,
    * if defined, otherwise returns this.
    * @group Recovering */
  def recover[X >: A](q: PartialFunction[B, X]): X Else B = macro ElseMacros.recover[X, B]

  /** Returns the binding of a function applied to the value of this `Trap`,
    * if defined, otherwise returns this.
    * @group Recovering */
  def recoverWith[X >: A, Y >: B](q: PartialFunction[B, X Else Y]): X Else Y = macro ElseMacros.recoverWith[X, B, Y]

  /** Returns this `Bind` if its value satisfies the predicate,
    * returns the unit `Trap` if its value does not satisfy the predicate,
    * otherwise returns this `Trap`.
    * @group Composing */
  def filter(p: A => Boolean): A Else B = macro ElseMacros.filter[A, B]

  /** Returns this `Bind` if its value satisfies the predicate,
    * returns the unit `Trap` if its value does not satisfy the predicate,
    * otherwise returns this `Trap`; equivalent to `filter`.
    * @group Composing */
  def withFilter(p: A => Boolean): A Else B = macro ElseMacros.filter[A, B]

  /** Selects [[FuseOps fused]] combinators that trap exceptions with the given handler.
    * @group Handling */
  def fuse[Y >: B](trip: Throwable => Trap[Y]): FuseOps[A, Y] = macro ElseMacros.fuse[A, Y]

  /** Selects [[FuseOps fused]] combinators that trap non-fatal exceptions.
    * @group Handling */
  def fuse(implicit isTry: B <:< Throwable): FuseOps[A, Throwable] = macro ElseMacros.fuseTry[A]
}

private[util] class ElseMacros(val c: blackbox.Context { type PrefixType <: ElseOps[_, _] }) {
  import c.{ Expr, mirror, prefix, weakTypeOf, WeakTypeTag }
  import c.universe._

  def bindOrElse[X : WeakTypeTag, B](default: Expr[X]): Expr[X] = Expr[X](q"""{
    val r = $prefix.__
    if (r.canBind) r.bind else $default
  }""")

  def orElse[X, Y](other: Expr[X Else Y])(implicit X: WeakTypeTag[X], Y: WeakTypeTag[Y]): Expr[X Else Y] = Expr[X Else Y](q"""{
    val r = $prefix.__
    if (r.canBind) r.asInstanceOf[$X Else Nothing] else $other
  }""")

  def orNull[X : WeakTypeTag](isNullable: Expr[Null <:< X]): Expr[X] = Expr[X](q"""{
    val r = $prefix.__
    if (r.canBind) r.bind else null
  }""")

  def fold[A, X : WeakTypeTag](z: Expr[X])(f: Expr[A => X]): Expr[X] = Expr[X](q"""{
    val r = $prefix.__
    if (r.canBind) $f(r.bind) else $z
  }""")

  def exists[A](p: Expr[A => Boolean]): Expr[Boolean] = Expr[Boolean](q"""{
    val r = $prefix.__
    r.canBind && $p(r.bind)
  }""")

  def foreach[A, U](f: Expr[A => U]): Expr[Unit] = Expr[Unit](q"""{
    val r = $prefix.__
    if (r.canBind) $f(r.bind)
  }""")

  def map[A, X, B](f: Expr[A => X])(implicit X: WeakTypeTag[X], B: WeakTypeTag[B]): Expr[X Else B] = Expr[X Else B](q"""{
    val r = $prefix.__
    if (r.canBind) _root_.basis.util.Bind($f(r.bind)) else r.asInstanceOf[Nothing Else $B]
  }""")

  def flatMap[A, X, Y](f: Expr[A => (X Else Y)])(implicit X: WeakTypeTag[X], Y: WeakTypeTag[Y]): Expr[X Else Y] = Expr[X Else Y](q"""{
    val r = $prefix.__
    if (r.canBind) $f(r.bind) else r.asInstanceOf[Nothing Else $Y]
  }""")

  def recover[X : WeakTypeTag, B : WeakTypeTag](q: Expr[PartialFunction[B, X]]): Expr[X Else B] = Expr[X Else B](q"""{
    val r = $prefix.__
    val q = $q
    if (r.canSafelyTrap && q.isDefinedAt(r.trap)) _root_.basis.util.Bind(q.applyOrElse(r.trap, _root_.scala.PartialFunction.empty)) else r
  }""")

  def recoverWith[X : WeakTypeTag, B : WeakTypeTag, Y >: B : WeakTypeTag](q: Expr[PartialFunction[B, X Else Y]]): Expr[X Else Y] = Expr[X Else Y](q"""{
    val r = $prefix.__
    if (r.canSafelyTrap) $q.applyOrElse(r.trap, (_: B) => r) else r
  }""")

  def filter[A : WeakTypeTag, B : WeakTypeTag](p: Expr[A => Boolean]): Expr[A Else B] = Expr[A Else B](q"""{
    val r = $prefix.__
    if (r.canTrap || $p(r.bind)) r else _root_.basis.util.Trap
  }""")

  def fuse[A : WeakTypeTag, Y : WeakTypeTag](trip: Expr[Throwable => Trap[Y]]): Expr[FuseOps[A, Y]] = Expr[FuseOps[A, Y]](q"new _root_.basis.util.FuseOps($prefix, $trip)")
  def fuseTry[A : WeakTypeTag](isTry: Expr[_ <:< Throwable]): Expr[FuseOps[A, Throwable]]           = Expr[FuseOps[A, Throwable]](q"new _root_.basis.util.FuseOps($prefix, _root_.basis.util.Trap.NonFatal)")

  implicit protected def ElseTag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[A Else B]         = WeakTypeTag(appliedType(mirror.staticClass("basis.util.Else").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
  implicit protected def TrapTag[B : WeakTypeTag]: WeakTypeTag[Trap[B]]                           = WeakTypeTag(appliedType(mirror.staticClass("basis.util.Trap").toType, weakTypeOf[B] :: Nil))
  implicit protected def FuseOpsTag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[FuseOps[A, B]] = WeakTypeTag(appliedType(mirror.staticClass("basis.util.FuseOps").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
  implicit protected def ThrowableTag: WeakTypeTag[Throwable]                                     = WeakTypeTag(mirror.staticClass("java.lang.Throwable").toType)
}
