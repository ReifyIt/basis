/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

import Predef.<:<

/** Standard [[Option]] value combinators.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating  1
  * @groupprio  Composing   2
  */
final class OptionOps[+A](option: Option[A]) {
  /** Returns `Some` value, or the `default` value if `None`.
    * @group Evaluating */
  def getOrElse[B >: A](default: B): B =
    macro OptionMacros.getOrElse[B]
  
  /** Returns `this` for `Some`, or `alternate` if `None`.
    * @group Composing */
  def orElse[B >: A](alternate: Option[B]): Option[B] =
    macro OptionMacros.orElse[B]
  
  /** Returns `Some` reference, or `null` if `None`.
    * @group Evaluating */
  def orNull[B >: A](implicit isNullable: Null <:< B): B =
    macro OptionMacros.orNull[B]
  
  /** Returns the given function applied to `Some` value, or evaluates `zero` if `None`.
    * @group Evaluating */
  def fold[B](zero: B)(f: A => B): B =
    macro OptionMacros.fold[A, B]
  
  /** Applies the given side-effecting function only to `Some` value.
    * @group Evaluating */
  def foreach[U](f: A => U): Unit =
    macro OptionMacros.foreach[A, U]
  
  /** Returns `Some` given function applied to `Some` value, otherwise `None`.
    * @group Composing */
  def map[B](f: A => B): Option[B] =
    macro OptionMacros.map[A, B]
  
  /** Returns the given `Option` function applied to `Some` value, otherwise `None`.
    * @group Composing */
  def flatMap[B](f: A => Option[B]): Option[B] =
    macro OptionMacros.flatMap[A, B]
  
  /** Returns `Some` value that satisfies the given predicate, otherwise `None`.
    * @group Composing */
  def filter(p: A => Boolean): Option[A] =
    macro OptionMacros.filter[A]
  
  /** Returns `Some` value that satisfies the given predicate, otherwise `None`;
    * equivalent to `filter`.
    * @group Composing */
  def withFilter(p: A => Boolean): Option[A] =
    macro OptionMacros.filter[A]
}

private[control] object OptionMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Option[A]] = {
    import c.{Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val Apply(_, option :: Nil) = prefix.tree
    val OptionType = appliedType(mirror.staticClass("basis.control.Option").toType, weakTypeOf[A] :: Nil)
    Expr(typeCheck(option, OptionType))(WeakTypeTag(OptionType))
  }
  
  def OptionOps[A : c.WeakTypeTag](c: Context)(option: c.Expr[Option[A]]): c.Expr[OptionOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val OptionOpsType = appliedType(mirror.staticClass("basis.control.OptionOps").toType, weakTypeOf[A] :: Nil)
    Expr(New(OptionOpsType, option.tree))(WeakTypeTag(OptionOpsType))
  }
  
  def getOrElse[A : c.WeakTypeTag](c: Context)(default: c.Expr[A]): c.Expr[A] = {
    import c.{Expr, fresh, weakTypeTag}
    import c.universe._
    var m = newTermName(fresh("option$"))
    Expr {
      Block(
        ValDef(NoMods, m, TypeTree(), unApply[A](c).tree) :: Nil,
        If(Select(Ident(m), "isDefined"), Select(Ident(m), "get"), default.tree))
    } (weakTypeTag[A])
  }
  
  def orElse[A : c.WeakTypeTag](c: Context)(alternate: c.Expr[Option[A]]): c.Expr[Option[A]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val m = newTermName(fresh("option$"))
    val OptionType = appliedType(mirror.staticClass("basis.control.Option").toType, weakTypeOf[A] :: Nil)
    Expr {
      Block(
        ValDef(NoMods, m, TypeTree(), unApply[A](c).tree) :: Nil,
        If(Select(Ident(m), "isDefined"), Ident(m), alternate.tree))
    } (WeakTypeTag(OptionType))
  }
  
  def orNull[A : c.WeakTypeTag](c: Context)(isNullable: c.Expr[Null <:< A]): c.Expr[A] = {
    import c.{Expr, fresh, weakTypeTag}
    import c.universe._
    var m = newTermName(fresh("option$"))
    Expr {
      Block(
        ValDef(NoMods, m, TypeTree(), unApply[A](c).tree) :: Nil,
        If(Select(Ident(m), "isDefined"), Select(Ident(m), "get"), Literal(Constant(null))))
    } (weakTypeTag[A])
  }
  
  def fold[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(zero: c.Expr[B])(f: c.Expr[A => B]): c.Expr[B] = {
    import c.{Expr, fresh, weakTypeTag}
    import c.universe._
    val m = newTermName(fresh("option$"))
    Expr {
      Block(
        ValDef(NoMods, m, TypeTree(), unApply[A](c).tree) :: Nil,
        If(Select(Ident(m), "isDefined"), Apply(f.tree, Select(Ident(m), "get") :: Nil), zero.tree))
    } (weakTypeTag[B])
  }
  
  def foreach[A : c.WeakTypeTag, U](c: Context)(f: c.Expr[A => U]): c.Expr[Unit] = {
    import c.{Expr, fresh, TypeTag}
    import c.universe._
    val m = newTermName(fresh("option$"))
    Expr {
      Block(
        ValDef(NoMods, m, TypeTree(), unApply[A](c).tree) :: Nil,
        If(Select(Ident(m), "isDefined"), Apply(f.tree, Select(Ident(m), "get") :: Nil), EmptyTree))
    } (TypeTag.Unit)
  }
  
  def map[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(f: c.Expr[A => B]): c.Expr[Option[B]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val m = newTermName(fresh("option$"))
    val OptionType = appliedType(mirror.staticClass("basis.control.Option").toType, weakTypeOf[B] :: Nil)
    Expr {
      Block(
        ValDef(NoMods, m, TypeTree(), unApply[A](c).tree) :: Nil,
        If(
          Select(Ident(m), "isDefined"),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Some"),
                Apply(f.tree, Select(Ident(m), "get") :: Nil) :: Nil),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "None")))
    } (WeakTypeTag(OptionType))
  }
  
  def flatMap[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(f: c.Expr[A => Option[B]]): c.Expr[Option[B]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val m = newTermName(fresh("option$"))
    val OptionType = appliedType(mirror.staticClass("basis.control.Option").toType, weakTypeOf[B] :: Nil)
    Expr {
      Block(
        ValDef(NoMods, m, TypeTree(), unApply[A](c).tree) :: Nil,
        If(
          Select(Ident(m), "isDefined"),
          Apply(f.tree, Select(Ident(m), "get") :: Nil),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "None")))
    } (WeakTypeTag(OptionType))
  }
  
  def filter[A : c.WeakTypeTag](c: Context)(p: c.Expr[A => Boolean]): c.Expr[Option[A]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val m = newTermName(fresh("option$"))
    val OptionType = appliedType(mirror.staticClass("basis.control.Option").toType, weakTypeOf[A] :: Nil)
    Expr {
      Block(
        ValDef(NoMods, m, TypeTree(), unApply[A](c).tree) :: Nil,
        If(
          Apply(Select(Select(Ident(m), "isDefined"), "$amp$amp"), Apply(p.tree, Select(Ident(m), "get") :: Nil) :: Nil),
          Ident(m),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "None")))
    } (WeakTypeTag(OptionType))
  }
}
