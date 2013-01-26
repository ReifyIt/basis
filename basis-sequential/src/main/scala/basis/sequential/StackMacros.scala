/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._
import basis.control._

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

/** Stack operations macro implementations.
  * 
  * @author Chris Sachs
  */
private[sequential] class StackMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def foreach[A : WeakTypeTag, U]
      (these: Expr[Stack[A]])
      (f: Expr[A => U])
    : Expr[Unit] = {
    val xs   = newTermName(fresh("xs$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Unit](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) :: Nil,
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(f.tree, Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree))))
  }
  
  def foldLeft[A : WeakTypeTag, B : WeakTypeTag]
      (these: Expr[Stack[A]])
      (z: Expr[B])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[B](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def reduceLeft[A : WeakTypeTag, B >: A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[B](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
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
        Ident(r)))
  }
  
  def mayReduceLeft[A : WeakTypeTag, B >: A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (op: Expr[(B, A) => B])
    : Expr[Maybe[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Maybe[B]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) :: Nil,
        If(
          Select(Ident(xs), "isEmpty"),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Trap"),
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
            Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Free"), Ident(r) :: Nil)))))
  }
  
  def find[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
    : Expr[Maybe[A]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val MaybeATag = MaybeTag[A]
    Expr[Maybe[A]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[Maybe[A]]),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Trap")) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Free"), Ident(x) :: Nil)),
                Block(
                  Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def forall[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Boolean](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
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
        Ident(r)))
  }
  
  def exists[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Boolean](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
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
        Ident(r)))
  }
  
  def count[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = newTermName(fresh("xs$"))
    val t    = newTermName(fresh("t$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Int](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
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
        Ident(t)))
  }
  
  def choose[A : WeakTypeTag, B : WeakTypeTag]
      (these: Expr[Stack[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Maybe[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val f    = newTermName(fresh("q$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val MaybeBTag = MaybeTag[B]
    Expr[Maybe[B]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[Maybe[B]]),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Trap")) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) :: Nil,
              If(
                Apply(Select(Ident(f), "isDefinedAt"), Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  Apply(
                    Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Free"),
                    Apply(Select(Ident(f), "applyOrElse"), Ident(x) ::
                      Select(Select(Select(Ident(nme.ROOTPKG), "scala"), "PartialFunction"), "empty") :: Nil) :: Nil)),
                Block(
                  Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def collect[A : WeakTypeTag, B]
      (these: Expr[Stack[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val f    = newTermName(fresh("q$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              If(
                Apply(Select(Ident(f), "isDefinedAt"), Ident(x) :: Nil),
                Apply(
                  Select(Ident(b), "append"),
                  Apply(Select(Ident(f), "applyOrElse"), Ident(x) ::
                    Select(Select(Select(Ident(nme.ROOTPKG), "scala"), "PartialFunction"), "empty") :: Nil) :: Nil),
                EmptyTree) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))
  }
  
  def map[A : WeakTypeTag, B]
      (these: Expr[Stack[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "append"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))
  }
  
  def flatMap[A : WeakTypeTag, B]
      (these: Expr[Stack[A]])
      (f: Expr[A => Enumerator[B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "appendAll"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))
  }
  
  def filter[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def dropWhile[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def takeWhile[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def span[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (p: Expr[A => Boolean])
      (builder1: Expr[Builder[_, A]], builder2: Expr[Builder[_, A]])
    : Expr[(builder1.value.State, builder2.value.State)] = {
    val xs    = newTermName(fresh("xs$"))
    val a     = newTermName(fresh("b$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    implicit val builder1TypeTag = BuilderTypeTag(builder1)
    implicit val builder1StateTag = BuilderStateTag(builder1)
    implicit val builder2TypeTag = BuilderTypeTag(builder2)
    implicit val builder2StateTag = BuilderStateTag(builder2)
    Expr[(builder1.value.State, builder2.value.State)](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(NoMods, a, TypeTree(builder1TypeTag.tpe), builder1.tree) ::
        ValDef(NoMods, b, TypeTree(builder2TypeTag.tpe), builder2.tree) ::
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
          Select(Ident(a), "state") :: Select(Ident(b), "state") :: Nil)))
  }
  
  def drop[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def take[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (upper: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), upper.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def slice[A : WeakTypeTag]
      (these: Expr[Stack[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(Modifiers(Flag.MUTABLE), n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def zip[A : WeakTypeTag, B : WeakTypeTag]
      (these: Expr[Stack[A]], those: Expr[Stack[B]])
      (builder: Expr[Builder[_, (A, B)]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val ys   = newTermName(fresh("ys$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(FamilyTag(these).tpe), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), ys, TypeTree(FamilyTag(those).tpe), those.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  protected[this] def BuilderTypeTag(builder: Expr[Builder[_, _]]): WeakTypeTag[builder.value.type] =
    WeakTypeTag[builder.value.type](builder.tree.symbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => builder.actualType
    })
  
  protected[this] def BuilderStateTag
      (builder: Expr[Builder[_, _]])
      (implicit BuilderTypeTag: WeakTypeTag[builder.value.type])
    : WeakTypeTag[builder.value.State] = {
    val BuilderTpc = mirror.staticClass("basis.collections.Builder").toType
    val BuilderStateSym = BuilderTpc member newTypeName("State")
    val BuilderStateTpe = typeRef(BuilderTypeTag.tpe, BuilderStateSym, Nil)
    WeakTypeTag[builder.value.State](BuilderStateTpe)
  }
  
  protected[this] def FamilyTag(these: Expr[Family[_]]): WeakTypeTag[these.value.Family] = {
    val TheseTpe = these.tree.symbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => these.actualType
    }
    val FamilyTpc = mirror.staticClass("basis.collections.Family").toType
    val FamilySym = FamilyTpc member newTypeName("Family")
    val FamilyTpe = typeRef(TheseTpe, FamilySym, Nil)
    WeakTypeTag[these.value.Family](FamilyTpe)
  }
  
  implicit protected[this] def MaybeTag[A : WeakTypeTag]: WeakTypeTag[Maybe[A]] = {
    val BasisControl = mirror.staticPackage("basis.control").moduleClass
    val MaybeTpc = BasisControl.typeSignature.member(newTypeName("Maybe")).asType.toType
    val MaybeATpe = appliedType(MaybeTpc, weakTypeOf[A] :: Nil)
    WeakTypeTag[Maybe[A]](MaybeATpe)
  }
  
  implicit private[this] def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] = {
    val Tuple2Tpc = mirror.staticClass("scala.Tuple2").toType
    val Tuple2ABTpe = appliedType(Tuple2Tpc, weakTypeOf[A] :: weakTypeOf[B] :: Nil)
    WeakTypeTag[(A, B)](Tuple2ABTpe)
  }
}
