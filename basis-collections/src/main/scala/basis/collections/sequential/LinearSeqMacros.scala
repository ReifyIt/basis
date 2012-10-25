/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

private[collections] object LinearSeqMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def foreach[A : c.WeakTypeTag, U]
      (c: Context)
      (seq: c.Tree, f: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) :: Nil,
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            Apply(f, Select(Ident(xs), "head") :: Nil) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop), Nil)),
          EmptyTree)))
  }
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, z: c.Tree, op: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), z) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            Assign(Ident(r), Apply(op, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop), Nil)),
          EmptyTree)) :: Nil,
      Ident(r))
  }
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, op: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
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
            Assign(Ident(r), Apply(op, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop), Nil)),
          EmptyTree)) :: Nil,
      Ident(r))
  }
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, op: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) :: Nil,
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
                Assign(Ident(r), Apply(op, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree)) :: Nil,
          ApplyConstructor(
            Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
            Ident(r) :: Nil))))
  }
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, p: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      ValDef(Modifiers(Flag.MUTABLE), r, OptionTpe[A](c), Select(Select(Ident(nme.ROOTPKG), "scala"), "None")) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(xs), "head")) :: Nil,
            If(
              Apply(p, Ident(x) :: Nil),
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
  }
  
  def forall[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, p: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          If(
            Apply(p, Select(Ident(xs), "head") :: Nil),
            Block(
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            Assign(Ident(r), Literal(Constant(false)))),
          EmptyTree)) :: Nil,
      Ident(r))
  }
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, p: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          If(
            Apply(p, Select(Ident(xs), "head") :: Nil),
            Assign(Ident(r), Literal(Constant(true))),
            Block(
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil))),
          EmptyTree)) :: Nil,
      Ident(r))
  }
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, p: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val t    = c.fresh(newTermName("total$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            If(
              Apply(p, Select(Ident(xs), "head") :: Nil),
              Assign(Ident(t), Apply(Select(Ident(t), "$plus"), Literal(Constant(1)) :: Nil)),
              EmptyTree) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop), Nil)),
          EmptyTree)) :: Nil,
      Ident(t))
  }
  
  def select[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, q: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      ValDef(Modifiers(Flag.MUTABLE), r, OptionTpe[B](c), Select(Select(Ident(nme.ROOTPKG), "scala"), "None")) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(xs), "head")) :: Nil,
            If(
              Apply(Select(q, "isDefinedAt"), Ident(x) :: Nil),
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
  }
  
  def collect[A : c.WeakTypeTag, B]
      (c: Context)
      (seq: c.Tree, q: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(xs), "head")) ::
            If(
              Apply(Select(q, "isDefinedAt"), Ident(x) :: Nil),
              Apply(Select(Ident(b), "$plus$eq"), Apply(q, Ident(x) :: Nil) :: Nil),
              EmptyTree) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop), Nil)),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def map[A : c.WeakTypeTag, B]
      (c: Context)
      (seq: c.Tree, f: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            Apply(Select(Ident(b), "$plus$eq"), Apply(f, Select(Ident(xs), "head") :: Nil) :: Nil) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop), Nil)),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def flatMap[A : c.WeakTypeTag, B]
      (c: Context)
      (seq: c.Tree, f: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            Apply(Select(Ident(b), "$plus$plus$eq"), Apply(f, Select(Ident(xs), "head") :: Nil) :: Nil) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop), Nil)),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def filter[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, p: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(xs), "head")) ::
            If(
              Apply(p, Ident(x) :: Nil),
              Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil),
              EmptyTree) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop), Nil)),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def dropWhile[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, p: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs    = c.fresh(newTermName("xs$"))
    val b     = BufferName(c)(buffer)
    val loop1 = c.fresh(newTermName("loop$"))
    val x     = c.fresh(newTermName("head$"))
    val loop2 = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop1, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(xs), "head")) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            If(
              Apply(p, Ident(x) :: Nil),
              Apply(Ident(loop1), Nil),
              Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil))),
          EmptyTree)) ::
      LabelDef(loop2, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop2), Nil)),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def takeWhile[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, p: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(xs), "head")) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            If(
              Apply(p, Ident(x) :: Nil),
              Block(
                Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree)),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def span[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, p: c.Tree, bufferA: c.Tree, bufferB: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs    = c.fresh(newTermName("xs$"))
    val a     = BufferName(c)(bufferA)
    val b     = BufferName(c)(bufferB)
    val loop1 = c.fresh(newTermName("loop$"))
    val x     = c.fresh(newTermName("head$"))
    val loop2 = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(bufferA, a) ::
      BufferDef(c)(bufferB, b) ::
      LabelDef(loop1, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(xs), "head")) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            If(
              Apply(p, Ident(x) :: Nil),
              Block(
                Apply(Select(Ident(a), "$plus$eq"), Ident(x) :: Nil) :: Nil,
                Apply(Ident(loop1), Nil)),
              Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil))),
          EmptyTree)) ::
      LabelDef(loop2, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          Block(
            Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop2), Nil)),
          EmptyTree)) :: Nil,
      ApplyConstructor(
        Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
        Select(Ident(a), "state") :: Select(Ident(b), "state") :: Nil))
  }
  
  def drop[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, lower: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs    = c.fresh(newTermName("xs$"))
    val b     = BufferName(c)(buffer)
    val i     = c.fresh(newTermName("i$"))
    val n     = c.fresh(newTermName("n$"))
    val loop1 = c.fresh(newTermName("loop$"))
    val loop2 = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(buffer, b) ::
      ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
      ValDef(NoMods, n, TypeTree(), lower) ::
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
            Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
            Apply(Ident(loop2), Nil)),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def take[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, upper: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val b    = BufferName(c)(buffer)
    val i    = c.fresh(newTermName("i$"))
    val n    = c.fresh(newTermName("n$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(buffer, b) ::
      ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
      ValDef(NoMods, n, TypeTree(), upper) ::
      LabelDef(loop, Nil,
        If(
          Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def slice[A : c.WeakTypeTag]
      (c: Context)
      (seq: c.Tree, lower: c.Tree, upper: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs    = c.fresh(newTermName("xs$"))
    val b     = BufferName(c)(buffer)
    val i     = c.fresh(newTermName("i$"))
    val n     = c.fresh(newTermName("n$"))
    val loop1 = c.fresh(newTermName("loop$"))
    val loop2 = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), seq) ::
      BufferDef(c)(buffer, b) ::
      ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
      ValDef(Modifiers(Flag.MUTABLE), n, TypeTree(), lower) ::
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
      Assign(Ident(n), upper) ::
      LabelDef(loop2, Nil,
        If(
          Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (these: c.Tree, those: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val xs   = c.fresh(newTermName("xs$"))
    val ys   = c.fresh(newTermName("ys$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(Modifiers(Flag.MUTABLE), xs, LinearSeqTpe[A](c), these) ::
      ValDef(Modifiers(Flag.MUTABLE), ys, LinearSeqTpe[B](c), those) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
          If(
            Select(Select(Ident(ys), "isEmpty"), "unary_$bang"),
            Block(
              Apply(
                Select(Ident(b), "$plus$eq"),
                ApplyConstructor(
                  Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
                  Select(Ident(xs), "head") :: Select(Ident(ys), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) ::
              Assign(Ident(ys), Select(Ident(ys), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree),
          EmptyTree)) :: Nil,
      Select(Ident(b), "result"))
  }
  
  def ++ [A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (these: c.Tree, those: c.Tree, buffer: c.Tree)
    : c.Tree = {
    import c.universe._
    val b = BufferName(c)(buffer)
    Block(
      BufferDef(c)(buffer, b) ::
      Apply(Select(Ident(b), "$plus$plus$eq"), these :: Nil) ::
      Apply(Select(Ident(b), "$plus$plus$eq"), those :: Nil) :: Nil,
      Select(Ident(b), "result"))
  }
  
  private def LinearSeqTpe[A : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    AppliedTypeTree(
      Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "collections"), newTypeName("LinearSeq")),
      TypeTree(weakTypeOf[A]) :: Nil)
  }
  
  private def OptionTpe[A : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    AppliedTypeTree(
      Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Option")),
      TypeTree(weakTypeOf[A]) :: Nil)
  }
  
  private def BufferName(c: Context)(buffer: c.Tree): c.TermName = {
    import c.universe._
    buffer match {
      case Ident(stable) => stable.toTermName
      case _ => c.fresh(newTermName("buffer$"))
    }
  }
  
  private def BufferDef(c: Context)(buffer: c.Tree, name: c.TermName): c.Tree = {
    import c.universe._
    buffer match {
      case Ident(_) => EmptyTree
      case _ => ValDef(NoMods, name, TypeTree(), buffer)
    }
  }
}
