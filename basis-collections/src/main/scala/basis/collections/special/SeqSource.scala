//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package special

trait SeqSource[+CC, -A] extends CollectionSource[CC, A] {
  def fill(count: Int)(elem: => A): CC =
    macro SeqSource.fill[CC, A]

  def tabulate(count: Int)(f: Int => A): CC =
    macro SeqSource.tabulate[CC, A]

  def iterate(start: A, count: Int)(f: A => A): CC =
    macro SeqSource.iterate[CC, A]
}

private[special] object SeqSource {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  def fill[CC, A]
      (c: Context { type PrefixType <: SeqSource[CC, A] })
      (count: c.Expr[Int])
      (elem: c.Expr[A])
      (implicit CCTag: c.WeakTypeTag[CC])
    : c.Expr[CC] = {
    import c.{ Expr, fresh, prefix }
    import c.universe._
    val i    = fresh("i$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[CC](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(
            Select(Apply(Select(prefix.tree, "Builder": TermName), Nil), "expect": TermName),
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

  def tabulate[CC, A]
      (c: Context { type PrefixType <: SeqSource[CC, A] })
      (count: c.Expr[Int])
      (f: c.Expr[Int => A])
      (implicit CCTag: c.WeakTypeTag[CC])
    : c.Expr[CC] = {
    import c.{ Expr, fresh, prefix }
    import c.universe._
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[CC](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(
            Select(Apply(Select(prefix.tree, "Builder": TermName), Nil), "expect": TermName),
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

  def iterate[CC, A]
      (c: Context { type PrefixType <: SeqSource[CC, A] })
      (start: c.Expr[A], count: c.Expr[Int])
      (f: c.Expr[A => A])
      (implicit CCTag: c.WeakTypeTag[CC])
    : c.Expr[CC] = {
    import c.{ Expr, fresh, prefix }
    import c.universe._
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val a    = fresh("a$"): TermName
    val i    = fresh("i$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[CC](
      Block(
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(
            Select(Apply(Select(prefix.tree, "Builder": TermName), Nil), "expect": TermName),
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
}
