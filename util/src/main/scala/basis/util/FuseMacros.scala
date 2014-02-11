//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.collection.immutable.{ ::, Nil }
import scala.reflect.macros.Context

private[util] class FuseMacros[C <: Context](c: C) extends ElseMacros[C](c) {
  import context.{ Expr, fresh, mirror, WeakTypeTag }
  import universe.{ Bind => _, Try => _, _ }

  def fuse[A : WeakTypeTag, B : WeakTypeTag]
      (expr: Expr[A Else B])
      (trip: Expr[Throwable => Trap[B]])
    : Expr[A Else B] = {
    val e = fresh("e$"): TermName
    Expr[A Else B](
      universe.Try(
        expr.tree,
        CaseDef(
          universe.Bind(e, Typed(Ident(nme.WILDCARD), TypeTree(weakTypeOf[Throwable]))),
          EmptyTree,
          Apply(trip.tree, Ident(e) :: Nil)) :: Nil,
        EmptyTree))
  }

  def fuseMap[A, X : WeakTypeTag, B : WeakTypeTag]
      (self: Expr[A Else B])
      (f: Expr[A => X])
      (trip: Expr[Throwable => Trap[B]])
    : Expr[X Else B] =
    fuse[X, B](map[A, X, B](self)(f))(trip)

  def fuseFlatMap[A, X : WeakTypeTag, Y : WeakTypeTag]
      (self: Expr[A Else Y])
      (f: Expr[A => (X Else Y)])
      (trip: Expr[Throwable => Trap[Y]])
    : Expr[X Else Y] =
    fuse[X, Y](flatMap[A, X, Y](self)(f))(trip)

  def fuseRecover[X : WeakTypeTag, B : WeakTypeTag]
      (self: Expr[X Else B])
      (q: Expr[PartialFunction[B, X]])
      (trip: Expr[Throwable => Trap[B]])
    : Expr[X Else B] =
    fuse[X, B](recover[X, B](self)(q))(trip)

  def fuseRecoverWith[X : WeakTypeTag, B : WeakTypeTag, Y >: B : WeakTypeTag]
      (self: Expr[X Else B])
      (q: Expr[PartialFunction[B, X Else Y]])
      (trip: Expr[Throwable => Trap[Y]])
    : Expr[X Else Y] =
    fuse[X, Y](recoverWith[X, B, Y](self)(q))(trip)

  def fuseFilter[A : WeakTypeTag, B : WeakTypeTag]
      (self: Expr[A Else B])
      (p: Expr[A => Boolean])
      (trip: Expr[Throwable => Trap[B]])
    : Expr[A Else B] =
    fuse[A, B](filter[A, B](self)(p))(trip)

  def fuseTry[A : WeakTypeTag](expr: Expr[A]): Expr[A Else Throwable] = {
    val TrapNonFatalTpe = mirror.staticModule("basis.util.Trap.NonFatal").moduleClass.asType.toType
    implicit val TrapNonFatalTag = WeakTypeTag[Trap.NonFatal.type](TrapNonFatalTpe)
    val TrapNonFatal =
      Expr[Trap.NonFatal.type](
        Select(Select(BasisUtil, "Trap": TermName), "NonFatal": TermName))
    implicit val BindATag = BindTag[A]
    val BindExpr =
      Expr[Bind[A]](
        Apply(
          Select(BasisUtil, "Bind": TermName),
          expr.tree :: Nil))
    fuse[A, Throwable](BindExpr)(TrapNonFatal)
  }

  implicit protected def ThrowableTag: WeakTypeTag[Throwable] =
    WeakTypeTag[Throwable](mirror.staticClass("java.lang.Throwable").toType)

  private def BasisUtil: Tree =
    Select(Select(Ident(nme.ROOTPKG), "basis": TermName), "util": TermName)
}

private[util] object FuseMacros {
  private def unApply[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)
    : (c.Expr[A Else B], c.Expr[Throwable => Trap[B]]) = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, self :: trip :: Nil) = prefix.tree
    implicit val AElseBTag =
      WeakTypeTag[A Else B](
        appliedType(
          mirror.staticClass("basis.util.Else").toType,
          weakTypeOf[A] :: weakTypeOf[B] :: Nil))
    implicit val ThrowableToTrapBTag =
      WeakTypeTag[Throwable => Trap[B]](
        appliedType(
          definitions.FunctionClass(1).asType.toType,
          mirror.staticClass("java.lang.Throwable").toType ::
          appliedType(
            mirror.staticClass("basis.util.Trap").toType,
            weakTypeOf[B] :: Nil) :: Nil))
    (Expr[A Else B](typeCheck(self, weakTypeOf[A Else B])),
     Expr[Throwable => Trap[B]](typeCheck(trip, weakTypeOf[Throwable => Trap[B]])))
  }

  def FuseToOps[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (self: c.Expr[A Else B])
      (trip: c.Expr[Throwable => Trap[B]])
    : c.Expr[FuseOps[A, B]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val FuseOpsABTag =
      WeakTypeTag[FuseOps[A, B]](
        appliedType(
          mirror.staticClass("basis.util.FuseOps").toType,
          weakTypeOf[A] :: weakTypeOf[B] :: Nil))
    Expr[FuseOps[A, B]](
      Apply(
        Select(New(TypeTree(weakTypeOf[FuseOps[A, B]])), nme.CONSTRUCTOR),
        self.tree :: trip.tree :: Nil))
  }

  def TryFuseToOps[A : c.WeakTypeTag]
      (c: Context)
      (self: c.Expr[A Else Throwable])
    : c.Expr[FuseOps[A, Throwable]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val FuseOpsAThrowableTag =
      WeakTypeTag[FuseOps[A, Throwable]](
        appliedType(
          mirror.staticClass("basis.util.FuseOps").toType,
          weakTypeOf[A] :: mirror.staticClass("java.lang.Throwable").toType :: Nil))
    Expr[FuseOps[A, Throwable]](
      Apply(
        Select(New(TypeTree(weakTypeOf[FuseOps[A, Throwable]])), nme.CONSTRUCTOR),
        self.tree :: Select(Select(BasisUtil(c), "Trap": TermName), "NonFatal": TermName) :: Nil))
  }

  def Try[A : c.WeakTypeTag]
      (c: Context)
      (expr: c.Expr[A])
    : c.Expr[A Else Throwable] =
    new FuseMacros[c.type](c).fuseTry[A](expr)

  def map[A : c.WeakTypeTag, X : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => X])
    : c.Expr[X Else B] = {
    val (self, trip) = unApply[A, B](c)
    new FuseMacros[c.type](c).fuseMap[A, X, B](self)(f)(trip)
  }

  def flatMap[A : c.WeakTypeTag, X : c.WeakTypeTag, Y : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => (X Else Y)])
    : c.Expr[X Else Y] = {
    val (self, trip) = unApply[A, Y](c)
    new FuseMacros[c.type](c).fuseFlatMap[A, X, Y](self)(f)(trip)
  }

  def recover[X : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[B, X]])
    : c.Expr[X Else B] = {
    val (self, trip) = unApply[X, B](c)
    new FuseMacros[c.type](c).fuseRecover[X, B](self)(q)(trip)
  }

  def recoverWith[X : c.WeakTypeTag, B : c.WeakTypeTag, Y >: B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[B, X Else Y]])
    : c.Expr[X Else Y] = {
    val (self, trip) = unApply[X, B](c)
    new FuseMacros[c.type](c).fuseRecoverWith[X, B, Y](self)(q)(trip)
  }

  def filter[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[A Else B] = {
    val (self, trip) = unApply[A, B](c)
    new FuseMacros[c.type](c).fuseFilter[A, B](self)(p)(trip)
  }

  private def BasisUtil(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Ident(nme.ROOTPKG), "basis": TermName), "util": TermName)
  }
}
