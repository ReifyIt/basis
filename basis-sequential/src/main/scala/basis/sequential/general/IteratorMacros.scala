/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package general

import basis.collections._

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

private[general] final class IteratorMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def foreach[A, U]
      (self: Expr[Iterator[A]])
      (f: Expr[A => U])
    : Expr[Unit] = {
    val xs   = newTermName(fresh("xs$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) :: Nil,
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(f.tree, Select(Ident(xs), "head") :: Nil) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)))
    } (TypeTag.Unit)
  }
  
  def foldLeft[A, B : WeakTypeTag]
      (self: Expr[Iterator[A]])
      (z: Expr[B])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeft[A, B >: A : WeakTypeTag]
      (self: Expr[Iterator[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        If(
          Select(Ident(xs), "isEmpty"),
          Throw(
            ApplyConstructor(
              Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), newTypeName("UnsupportedOperationException")),
              Literal(Constant("Empty reduce.")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head")) ::
        Apply(Select(Ident(xs), "step"), Nil) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeftOption[A, B >: A : WeakTypeTag]
      (self: Expr[Iterator[A]])
      (op: Expr[(B, A) => B])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) :: Nil,
        If(
          Select(Ident(xs), "isEmpty"),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None"),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head")) ::
            Apply(Select(Ident(xs), "step"), Nil) ::
            LabelDef(loop, Nil,
              If(
                Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
                Block(
                  Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
                  Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            ApplyConstructor(
              Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
              Ident(r) :: Nil))))
    } (OptionTag[B])
  }
  
  def find[A : WeakTypeTag]
      (self: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Option[A]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(OptionTag[A].tpe),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None")) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  ApplyConstructor(
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
                    Ident(x) :: Nil)),
                Block(
                  Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (OptionTag[A])
  }
  
  def forall[A]
      (self: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Apply(p.tree, Select(Ident(xs), "head") :: Nil),
              Block(
                Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                Apply(Ident(loop), Nil)),
              Assign(Ident(r), Literal(Constant(false)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def exists[A]
      (self: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Apply(p.tree, Select(Ident(xs), "head") :: Nil),
              Assign(Ident(r), Literal(Constant(true))),
              Block(
                Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                Apply(Ident(loop), Nil))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def count[A]
      (self: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = newTermName(fresh("xs$"))
    val t    = newTermName(fresh("total$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              If(
                Apply(p.tree, Select(Ident(xs), "head") :: Nil),
                Assign(Ident(t), Apply(Select(Ident(t), "$plus"), Literal(Constant(1)) :: Nil)),
                EmptyTree) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(t))
    } (TypeTag.Int)
  }
  
  def choose[A, B : WeakTypeTag]
      (self: Expr[Iterator[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(OptionTag[B].tpe),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None")) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) :: Nil,
              If(
                Apply(Select(q.tree, "isDefinedAt"), Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  ApplyConstructor(
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
                    Apply(q.tree, Ident(x) :: Nil) :: Nil)),
                Block(
                  Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (OptionTag[B])
  }
  
  implicit private def OptionTag[A : WeakTypeTag]: WeakTypeTag[Option[A]] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Option").toType, weakTypeOf[A] :: Nil))
}
