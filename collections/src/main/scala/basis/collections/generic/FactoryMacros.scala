//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import scala.reflect.macros._

private[generic] class FactoryMacros(val c: blackbox.Context) {
  import c.{ Expr, fresh, prefix, WeakTypeTag }
  import c.universe._

  def fill[CC[_], A](count: Expr[Int])(elem: Expr[A])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    val i    = fresh("i$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val CCA = WeakTypeTag[CC[A]](appliedType(CC.tpe, A.tpe :: Nil))
    Expr[CC[A]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(
            Select(Apply(TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(A.tpe) :: Nil), Nil), "expect": TermName),
            Ident(i) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), (">": TermName).encodedName), Literal(Constant(0)) :: Nil),
            Block(
              Apply(Select(Ident(b), ("+=": TermName).encodedName), elem.tree :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def iterate[CC[_], A](start: Expr[A], count: Expr[Int])(f: Expr[A => A])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val a    = fresh("a$"): TermName
    val i    = fresh("i$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val CCA = WeakTypeTag[CC[A]](appliedType(CC.tpe, A.tpe :: Nil))
    Expr[CC[A]](
      Block(
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(
            Select(Apply(TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(A.tpe) :: Nil), Nil), "expect": TermName),
            Ident(n) :: Nil)) ::
        If(
          Apply(Select(Ident(n), (">": TermName).encodedName), Literal(Constant(0)) :: Nil),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), a, TypeTree(), start.tree) ::
            Apply(Select(Ident(b), ("+=": TermName).encodedName), Ident(a) :: Nil) ::
            ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) :: Nil,
            LabelDef(loop, Nil,
              If(
                Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
                Block(
                  Assign(Ident(a), Apply(f.tree, Ident(a) :: Nil)) ::
                  Apply(Select(Ident(b), ("+=": TermName).encodedName), Ident(a) :: Nil) ::
                  Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree))),
          EmptyTree) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def tabulate[CC[_], A](count: Expr[Int])(f: Expr[Int => A])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val CCA = WeakTypeTag[CC[A]](appliedType(CC.tpe, A.tpe :: Nil))
    Expr[CC[A]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(
            Select(Apply(TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(A.tpe) :: Nil), Nil), "expect": TermName),
            Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), ("+=": TermName).encodedName), Apply(f.tree, Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
}
