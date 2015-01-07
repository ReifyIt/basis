//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

import scala.reflect.macros._

final class FuseOps[+A, +B](val __ : A Else B, val trip: Throwable => Trap[B]) {
  /** Binds the result of a function applied to the value of this `Bind`,
    * otherwise returns this `Trap` or a tripped exception.
    * @group Composing */
  def map[X](f: A => X): X Else B = macro FuseMacros.map[A, X, B]

  /** Returns the binding of a function applied to the value of this `Bind`,
    * otherwise returns this `Trap` or a tripped exception.
    * @group Composing */
  def flatMap[X, Y >: B](f: A => (X Else Y)): X Else Y = macro FuseMacros.flatMap[A, X, Y]

  /** Binds the result of a function applied to the value of this `Trap`,
    * if defined, otherwise returns this or a tripped exception.
    * @group Recovering */
  def recover[X >: A](q: PartialFunction[B, X]): X Else B = macro FuseMacros.recover[X, B]

  /** Returns the binding of a function applied to the value of this `Trap`,
    * if defined, otherwise returns this or a tripped exception.
    * @group Recovering */
  def recoverWith[X >: A, Y >: B](q: PartialFunction[B, X Else Y]): X Else Y = macro FuseMacros.recoverWith[X, B, Y]

  /** Returns this `Bind` if its value satisfies a predicate,
    * returns the unit `Trap` if its value does not satisfy the predicate,
    * otherwise returns this `Trap` or a tripped exception.
    * @group Composing */
  def filter(p: A => Boolean): A Else B = macro FuseMacros.filter[A, B]

  /** Returns this `Bind` if its value satisfies the predicate,
    * returns the unit `Trap` if its value does not satisfy the predicate,
    * otherwise returns this `Trap` or a tripped exception;
    * equivalent to `filter`.
    * @group Composing */
  def withFilter(p: A => Boolean): A Else B = macro FuseMacros.filter[A, B]
}

private[basis] class FuseMacros(val c: blackbox.Context { type PrefixType <: FuseOps[_, _] }) {
  import c.{ Expr, mirror, prefix, TERMmode, typecheck, weakTypeOf, WeakTypeTag }
  import c.universe._

  protected def unApply[A : WeakTypeTag, B : WeakTypeTag]: (Tree, Tree) = {
    val Apply(_, self :: trip :: Nil) = prefix.tree
    (typecheck(self, TERMmode, weakTypeOf[A Else B]), typecheck(trip, TERMmode, weakTypeOf[Throwable => Trap[B]]))
  }

  def map[A, X, B](f: Expr[A => X])(implicit A: WeakTypeTag[A], X: WeakTypeTag[X], B: WeakTypeTag[B]): Expr[X Else B] = {
    val (self, trip) = unApply[A, B]
    Expr[X Else B](q"""{
      val r = $self
      try if (r.canBind) _root_.basis.Bind($f(r.bind)) else r.asInstanceOf[_root_.basis.Else[Nothing, $B]]
      catch { case e: Throwable => $trip(e) }
    }""")
  }

  def flatMap[A, X, Y](f: Expr[A => (X Else Y)])(implicit A: WeakTypeTag[A], X: WeakTypeTag[X], Y: WeakTypeTag[Y]): Expr[X Else Y] = {
    val (self, trip) = unApply[A, Y]
    Expr[X Else Y](q"""{
      val r = $self
      try if (r.canBind) $f(r.bind) else r.asInstanceOf[_root_.basis.Else[Nothing, $Y]]
      catch { case e: Throwable => $trip(e) }
    }""")
  }

  def recover[X : WeakTypeTag, B : WeakTypeTag](q: Expr[PartialFunction[B, X]]): Expr[X Else B] = {
    val (self, trip) = unApply[X, B]
    Expr[X Else B](q"""{
      val r = $self
      val q = $q
      try if (r.canSafelyTrap && q.isDefinedAt(r.trap)) _root_.basis.Bind(q.applyOrElse(r.trap, _root_.scala.PartialFunction.empty)) else r
      catch { case e: Throwable => $trip(e) }
    }""")
  }

  def recoverWith[X : WeakTypeTag, B : WeakTypeTag, Y : WeakTypeTag](q: Expr[PartialFunction[B, X Else Y]]): Expr[X Else Y] = {
    val (self, trip) = unApply[X, B]
    Expr[X Else Y](q"""{
      val r = $self
      try if (r.canSafelyTrap) $q.applyOrElse(r.trap, (_: B) => r) else r
      catch { case e: Throwable => $trip(e) }
    }""")
  }

  def filter[A : WeakTypeTag, B : WeakTypeTag](p: Expr[A => Boolean]): Expr[A Else B] = {
    val (self, trip) = unApply[A, B]
    Expr[A Else B](q"""{
      val r = $self
      try if (r.canTrap || $p(r.bind)) r else _root_.basis.Trap
      catch { case e: Throwable => $trip(e) }
    }""")
  }

  implicit protected def ElseTag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[A Else B]       = WeakTypeTag(appliedType(mirror.staticClass("basis.Else").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
  implicit protected def TrapTag[B : WeakTypeTag]: WeakTypeTag[Trap[B]]                         = WeakTypeTag(appliedType(mirror.staticClass("basis.Trap").toType, weakTypeOf[B] :: Nil))
  implicit protected def ThrowableToTrapTag[B : WeakTypeTag]: WeakTypeTag[Throwable => Trap[B]] = WeakTypeTag(appliedType(definitions.FunctionClass(1).asType.toType, weakTypeOf[Throwable] :: weakTypeOf[Trap[B]] :: Nil))
  implicit protected def ThrowableTag: WeakTypeTag[Throwable]                                   = WeakTypeTag(mirror.staticClass("java.lang.Throwable").toType)
}
