/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential

import basis.collections._

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

private[sequential] final class StackMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def foreach[A : WeakTypeTag, U]
      (these: Expr[Stack[A]])
      (f: Expr[A => U])
    : Expr[Unit] = {
    val xs   = newTermName(fresh("xs$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) :: Nil,
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(f.tree, Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)))
    } (TypeTag.Unit)
  }
  
  def foldLeft[A : WeakTypeTag, B : WeakTypeTag]
      (these: Expr[Stack[A]])
      (z: Expr[B])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeft[A : WeakTypeTag, B >: A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        If(
          Select(Ident(xs), "isEmpty"),
          Throw(
            ApplyConstructor(
              Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), newTypeName("UnsupportedOperationException")),
              Literal(Constant("Empty reduce.")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head")) ::
        Assign(Ident(xs), Select(Ident(xs), "tail")) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeftOption[A : WeakTypeTag, B >: A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (op: Expr[(B, A) => B])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) :: Nil,
        If(
          Select(Ident(xs), "isEmpty"),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None"),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head")) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) ::
            LabelDef(loop, Nil,
              If(
                Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
                Block(
                  Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
                  Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            ApplyConstructor(
              Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
              Ident(r) :: Nil))))
    } (OptionTag[B])
  }
  
  def find[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
    : Expr[Option[A]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
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
                  Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (OptionTag[A])
  }
  
  def forall[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Apply(p.tree, Select(Ident(xs), "head") :: Nil),
              Block(
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop), Nil)),
              Assign(Ident(r), Literal(Constant(false)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def exists[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Apply(p.tree, Select(Ident(xs), "head") :: Nil),
              Assign(Ident(r), Literal(Constant(true))),
              Block(
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop), Nil))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def count[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = newTermName(fresh("xs$"))
    val t    = newTermName(fresh("total$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              If(
                Apply(p.tree, Select(Ident(xs), "head") :: Nil),
                Assign(Ident(t), Apply(Select(Ident(t), "$plus"), Literal(Constant(1)) :: Nil)),
                EmptyTree) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(t))
    } (TypeTag.Int)
  }
  
  def choose[A : WeakTypeTag, B : WeakTypeTag]
      (these: Expr[Stack[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
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
                  Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (OptionTag[B])
  }
  
  def collect[A : WeakTypeTag, B]
      (these: Expr[Stack[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              If(
                Apply(Select(q.tree, "isDefinedAt"), Ident(x) :: Nil),
                Apply(Select(Ident(b), "append"), Apply(q.tree, Ident(x) :: Nil) :: Nil),
                EmptyTree) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def map[A : WeakTypeTag, B]
      (these: Expr[Stack[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "append"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def flatMap[A : WeakTypeTag, B]
      (these: Expr[Stack[A]])
      (f: Expr[A => Enumerator[B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "appendAll"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def filter[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Select(Ident(b), "append"), Ident(x) :: Nil),
                EmptyTree) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def dropWhile[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("head$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Ident(loop1), Nil),
                Apply(Select(Ident(b), "append"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def takeWhile[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(b), "append"), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def span[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
      (builder1: Expr[Builder[_, A]], builder2: Expr[Builder[_, A]])
    : Expr[(builder1.value.State, builder2.value.State)] = {
    val xs    = newTermName(fresh("xs$"))
    val a     = newTermName(fresh("builder$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("head$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(NoMods, a, TypeTree(BuilderType(builder1)), builder1.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder2)), builder2.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(a), "append"), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop1), Nil)),
                Apply(Select(Ident(b), "append"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        ApplyConstructor(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
          Select(Ident(a), "state") :: Select(Ident(b), "state") :: Nil))
    } (Tuple2Tag(StateTag(builder1), StateTag(builder2)))
  }
  
  def drop[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop1), Nil)),
              EmptyTree),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def take[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (upper: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), upper.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
              Block(
                Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def slice[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(Modifiers(Flag.MUTABLE), n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop1), Nil)),
              EmptyTree),
            EmptyTree)) ::
        Assign(Ident(n), upper.tree) ::
        LabelDef(loop2, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
              Block(
                Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop2), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def zip[A : WeakTypeTag, B : WeakTypeTag]
      (these: Expr[Stack[A]], those: Expr[Stack[B]])
      (builder: Expr[Builder[_, (A, B)]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val ys   = newTermName(fresh("ys$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyType(these)), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), ys, TypeTree(FamilyType(those)), those.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Select(Select(Ident(ys), "isEmpty"), "unary_$bang"),
              Block(
                Apply(
                  Select(Ident(b), "append"),
                  ApplyConstructor(
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
                    Select(Ident(xs), "head") :: Select(Ident(ys), "head") :: Nil) :: Nil) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) ::
                Assign(Ident(ys), Select(Ident(ys), "tail")) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  private def FamilyType(these: Expr[Family[_]]): Type = {
    val TheseType = these.tree.symbol match {
      case symbol: TermSymbol if symbol.isStable => singleType(NoPrefix, symbol)
      case _ => these.actualType
    }
    val FamilySymbol = mirror.staticClass("basis.collections.Family").toType.member(newTypeName("Family"))
    typeRef(TheseType, FamilySymbol, Nil)
  }
  
  private def BuilderType(builder: Expr[Builder[_, _]]): Type = builder.tree.symbol match {
    case symbol: TermSymbol if symbol.isStable => singleType(NoPrefix, symbol)
    case _ => builder.actualType
  }
  
  private def StateTag(builder: Expr[Builder[_, _]]): WeakTypeTag[builder.value.State] = {
    val StateSymbol = mirror.staticClass("basis.collections.Builder").toType.member(newTypeName("State"))
    WeakTypeTag(typeRef(BuilderType(builder), StateSymbol, Nil))
  }
  
  private def OptionTag[A : WeakTypeTag]: WeakTypeTag[Option[A]] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Option").toType, weakTypeOf[A] :: Nil))
  
  private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Tuple2").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
}
