//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.collection.immutable.{ ::, Nil }
import scala.reflect.macros.Context
import scala.Predef.<:<

private[util] class ElseMacros[C <: Context](val context: C) {
  import context.{ Expr, fresh, mirror, WeakTypeTag }
  import universe.{ Bind => _, Try => _, _ }

  val universe: context.universe.type = context.universe

  def bindOrElse[X : WeakTypeTag]
      (self: Expr[X Else Any])
      (default: Expr[X])
    : Expr[X] = {
    val r = fresh("r$"): TermName
    Expr[X](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        If(
          Select(Ident(r), "canBind": TermName),
          Select(Ident(r), "bind": TermName),
          default.tree)))
  }

  def orElse[X : WeakTypeTag, Y : WeakTypeTag]
      (self: Expr[X Else Any])
      (other: Expr[X Else Y])
    : Expr[X Else Y] = {
    val r = fresh("r$"): TermName
    Expr[X Else Y](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        If(
          Select(Ident(r), "canBind": TermName),
          TypeApply(
            Select(Ident(r), "asInstanceOf": TermName),
            TypeTree(ElseTag[X, Nothing].tpe) :: Nil),
          other.tree)))
  }

  def orNull[X : WeakTypeTag]
      (self: Expr[X Else Any])
      (isNullable: Expr[Null <:< X])
    : Expr[X] = {
    val r = fresh("r$"): TermName
    Expr[X](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        If(
          Select(Ident(r), "canBind": TermName),
          Select(Ident(r), "bind": TermName),
          Literal(Constant(null)))))
  }

  def fold[A, X : WeakTypeTag]
      (self: Expr[A Else Any])
      (z: Expr[X])
      (f: Expr[A => X])
    : Expr[X] = {
    val r = fresh("r$"): TermName
    Expr[X](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        If(
          Select(Ident(r), "canBind": TermName),
          Apply(f.tree, Select(Ident(r), "bind": TermName) :: Nil),
          z.tree)))
  }

  def exists[A]
      (self: Expr[A Else Any])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val r = fresh("r$"): TermName
    Expr[Boolean](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        Apply(
          Select(Select(Ident(r), "canBind": TermName), ("&&": TermName).encodedName),
          Apply(p.tree, Select(Ident(r), "bind": TermName) :: Nil) :: Nil)))
  }

  def foreach[A, U]
      (self: Expr[A Else Any])
      (f: Expr[A => U])
    : Expr[Unit] = {
    val r = fresh("r$"): TermName
    Expr[Unit](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        If(
          Select(Ident(r), "canBind": TermName),
          Apply(f.tree, Select(Ident(r), "bind": TermName) :: Nil),
          EmptyTree)))
  }

  def map[A, X : WeakTypeTag, B : WeakTypeTag]
      (self: Expr[A Else B])
      (f: Expr[A => X])
    : Expr[X Else B] = {
    val r = fresh("r$"): TermName
    Expr[X Else B](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        If(
          Select(Ident(r), "canBind": TermName),
          Apply(
            Select(BasisUtil, "Bind": TermName),
            Apply(f.tree, Select(Ident(r), "bind": TermName) :: Nil) :: Nil),
          TypeApply(
            Select(Ident(r), "asInstanceOf": TermName),
            TypeTree(ElseTag[Nothing, B].tpe) :: Nil))))
  }

  def flatMap[A, X : WeakTypeTag, Y : WeakTypeTag]
      (self: Expr[A Else Y])
      (f: Expr[A => (X Else Y)])
    : Expr[X Else Y] = {
    val r = fresh("r$"): TermName
    Expr[X Else Y](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        If(
          Select(Ident(r), "canBind": TermName),
          Apply(f.tree, Select(Ident(r), "bind": TermName) :: Nil),
          TypeApply(
            Select(Ident(r), "asInstanceOf": TermName),
            TypeTree(ElseTag[Nothing, Y].tpe) :: Nil))))
  }

  def recover[X : WeakTypeTag, B : WeakTypeTag]
      (self: Expr[X Else B])
      (q: Expr[PartialFunction[B, X]])
    : Expr[X Else B] = {
    val r = fresh("r$"): TermName
    val f = fresh("q$"): TermName
    Expr[X Else B](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) ::
        ValDef(NoMods, f, TypeTree(), q.tree) :: Nil,
        If(
          Apply(
            Select(Select(Ident(r), "canSafelyTrap": TermName), ("&&": TermName).encodedName),
            Apply(
              Select(Ident(f), "isDefinedAt": TermName),
              Select(Ident(r), "trap": TermName) :: Nil) :: Nil),
          Apply(
            Select(BasisUtil, "Bind": TermName),
            Apply(
              Select(Ident(f), "applyOrElse": TermName),
              Select(Ident(r), "trap": TermName) ::
              Select(ScalaPartialFunction, "empty": TermName) :: Nil) :: Nil),
          Ident(r))))
  }

  def recoverWith[X : WeakTypeTag, B : WeakTypeTag, Y >: B : WeakTypeTag]
      (self: Expr[X Else B])
      (q: Expr[PartialFunction[B, X Else Y]])
    : Expr[X Else Y] = {
    val r = fresh("r$"): TermName
    Expr[X Else Y](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        If(
          Select(Ident(r), "canSafelyTrap": TermName),
          Apply(
            Select(q.tree, "applyOrElse": TermName),
            Select(Ident(r), "trap": TermName) ::
            Function(
              ValDef(Modifiers(Flag.PARAM), nme.WILDCARD, TypeTree(weakTypeOf[B]), EmptyTree) :: Nil,
              Ident(r)) :: Nil),
          Ident(r))))
  }

  def filter[A : WeakTypeTag, B : WeakTypeTag]
      (self: Expr[A Else B])
      (p: Expr[A => Boolean])
    : Expr[A Else B] = {
    val r = fresh("r$"): TermName
    Expr[A Else B](
      Block(
        ValDef(NoMods, r, TypeTree(), self.tree) :: Nil,
        If(
          Apply(
            Select(Select(Ident(r), "canTrap": TermName), ("||": TermName).encodedName),
            Apply(p.tree, Select(Ident(r), "bind": TermName) :: Nil) :: Nil),
          Ident(r),
          Select(BasisUtil, "Trap": TermName))))
  }

  implicit protected def ElseTag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[A Else B] =
    WeakTypeTag(appliedType(mirror.staticClass("basis.util.Else").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))

  implicit protected def BindTag[A : WeakTypeTag]: WeakTypeTag[Bind[A]] =
    WeakTypeTag(appliedType(mirror.staticClass("basis.util.Bind").toType, weakTypeOf[A] :: Nil))

  implicit protected def TrapTag[B : WeakTypeTag]: WeakTypeTag[Trap[B]] =
    WeakTypeTag(appliedType(mirror.staticClass("basis.util.Trap").toType, weakTypeOf[B] :: Nil))

  private def BasisUtil: Tree =
    Select(Select(Ident(nme.ROOTPKG), "basis": TermName), "util": TermName)

  private def ScalaPartialFunction: Tree =
    Select(Select(Ident(nme.ROOTPKG), "scala": TermName), "PartialFunction": TermName)
}

private[util] object ElseMacros {
  private def unApply[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context): c.Expr[A Else B] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, self :: Nil) = prefix.tree
    implicit val AElseBTag =
      WeakTypeTag[A Else B](
        appliedType(
          mirror.staticClass("basis.util.Else").toType,
          weakTypeOf[A] :: weakTypeOf[B] :: Nil))
    Expr[A Else B](typeCheck(self, weakTypeOf[A Else B]))
  }

  def ElseToOps[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (self: c.Expr[A Else B])
    : c.Expr[ElseOps[A, B]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val ElseOpsABTag =
      WeakTypeTag[ElseOps[A, B]](
        appliedType(
          mirror.staticClass("basis.util.ElseOps").toType,
          weakTypeOf[A] :: weakTypeOf[B] :: Nil))
    Expr[ElseOps[A, B]](
      Apply(
        Select(New(TypeTree(weakTypeOf[ElseOps[A, B]])), nme.CONSTRUCTOR),
        self.tree :: Nil))
  }

  def bindOrElse[X : c.WeakTypeTag]
      (c: Context)
      (default: c.Expr[X])
    : c.Expr[X] =
    new ElseMacros[c.type](c).bindOrElse[X](unApply[X, Any](c))(default)

  def orElse[X : c.WeakTypeTag, Y : c.WeakTypeTag]
      (c: Context)
      (other: c.Expr[X Else Y])
    : c.Expr[X Else Y] =
    new ElseMacros[c.type](c).orElse[X, Y](unApply[X, Any](c))(other)

  def orNull[X : c.WeakTypeTag]
      (c: Context)
      (isNullable: c.Expr[Null <:< X])
    : c.Expr[X] =
    new ElseMacros[c.type](c).orNull[X](unApply[X, Any](c))(isNullable)

  def fold[A : c.WeakTypeTag, X : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[X])
      (f: c.Expr[A => X])
    : c.Expr[X] =
    new ElseMacros[c.type](c).fold[A, X](unApply[A, Any](c))(z)(f)

  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new ElseMacros[c.type](c).exists[A](unApply[A, Any](c))(p)

  def foreach[A : c.WeakTypeTag, U]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] =
    new ElseMacros[c.type](c).foreach[A, U](unApply[A, Any](c))(f)

  def map[A : c.WeakTypeTag, X : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => X])
    : c.Expr[X Else B] =
    new ElseMacros[c.type](c).map[A, X, B](unApply[A, B](c))(f)

  def flatMap[A : c.WeakTypeTag, X : c.WeakTypeTag, Y : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => (X Else Y)])
    : c.Expr[X Else Y] =
    new ElseMacros[c.type](c).flatMap[A, X, Y](unApply[A, Y](c))(f)

  def recover[X : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[B, X]])
    : c.Expr[X Else B] =
    new ElseMacros[c.type](c).recover[X, B](unApply[X, B](c))(q)

  def recoverWith[X : c.WeakTypeTag, B : c.WeakTypeTag, Y >: B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[B, X Else Y]])
    : c.Expr[X Else Y] =
    new ElseMacros[c.type](c).recoverWith[X, B, Y](unApply[X, B](c))(q)

  def filter[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[A Else B] =
    new ElseMacros[c.type](c).filter[A, B](unApply[A, B](c))(p)

  def fuse[A : c.WeakTypeTag, Y : c.WeakTypeTag]
      (c: Context)
      (trip: c.Expr[Throwable => Trap[Y]])
    : c.Expr[FuseOps[A, Y]] =
    FuseMacros.FuseToOps[A, Y](c)(unApply[A, Y](c))(trip)

  def fuseTry[A : c.WeakTypeTag]
      (c: Context)
      (isTry: c.Expr[_ <:< Throwable])
    : c.Expr[FuseOps[A, Throwable]] = {
    val ThrowableTpe = c.mirror.staticClass("java.lang.Throwable").toType
    implicit val ThrowableTag = c.WeakTypeTag[Throwable](ThrowableTpe)
    FuseMacros.TryFuseToOps[A](c)(unApply[A, Throwable](c))
  }
}
