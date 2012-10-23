/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

private[sequential] object EagerIteratorMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def collect[A : c.WeakTypeTag, B](c: Context)(iterator: c.Tree, q: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(iter), "head")) ::
            If(
              Apply(Select(q, "isDefinedAt"), Ident(x) :: Nil),
              Apply(Select(Ident(b), "$plus$eq"), Apply(q, Ident(x) :: Nil) :: Nil),
              EmptyTree) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop), Nil)))) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def map[A : c.WeakTypeTag, B](c: Context)(iterator: c.Tree, f: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            Apply(Select(Ident(b), "$plus$eq"), Apply(f, Select(Ident(iter), "head") :: Nil) :: Nil) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop), Nil)))) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def flatMap[A : c.WeakTypeTag, B](c: Context)(iterator: c.Tree, f: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            Apply(Select(Ident(b), "$plus$plus$eq"), Apply(f, Select(Ident(iter), "head") :: Nil) :: Nil) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop), Nil)))) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def filter[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, p: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(iter), "head")) ::
            If(
              Apply(p, Ident(x) :: Nil),
              Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil),
              EmptyTree) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop), Nil)))) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def dropWhile[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, p: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter  = c.fresh(newTermName("iter$"))
    val b     = BufferName(c)(buffer)
    val loop1 = c.fresh(newTermName("loop$"))
    val x     = c.fresh(newTermName("head$"))
    val loop2 = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop1, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(iter), "head")) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            If(
              Apply(p, Ident(x) :: Nil),
              Apply(Ident(loop1), Nil),
              Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil))))) ::
      LabelDef(loop2, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            Apply(Select(Ident(b), "$plus$eq"), Select(Ident(iter), "head") :: Nil) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop2), Nil)))) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def takeWhile[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, p: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val b    = BufferName(c)(buffer)
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(iter), "head")) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            If(
              Apply(p, Ident(x) :: Nil),
              Block(
                Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree)))) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def span[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, p: c.Tree, bufferA: c.Tree, bufferB: c.Tree): c.Tree = {
    import c.universe._
    val iter  = c.fresh(newTermName("iter$"))
    val a     = BufferName(c)(bufferA)
    val b     = BufferName(c)(bufferB)
    val loop1 = c.fresh(newTermName("loop$"))
    val x     = c.fresh(newTermName("head$"))
    val loop2 = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(bufferA, a) ::
      BufferDef(c)(bufferB, b) ::
      LabelDef(loop1, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(iter), "head")) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            If(
              Apply(p, Ident(x) :: Nil),
              Block(
                Apply(Select(Ident(a), "$plus$eq"), Ident(x) :: Nil) :: Nil,
                Apply(Ident(loop1), Nil)),
              Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil))))) ::
      LabelDef(loop2, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            Apply(Select(Ident(b), "$plus$eq"), Select(Ident(iter), "head") :: Nil) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop2), Nil)))) :: Nil,
      NewTuple2(c)(Select(Ident(a), "state"), Select(Ident(b), "State")))
  }
  
  def drop[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, lower: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter  = c.fresh(newTermName("iter$"))
    val b     = BufferName(c)(buffer)
    val i     = c.fresh(newTermName("i$"))
    val n     = c.fresh(newTermName("n$"))
    val loop1 = c.fresh(newTermName("loop$"))
    val loop2 = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(buffer, b) ::
      ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
      ValDef(NoMods, n, TypeTree(), lower) ::
      LabelDef(loop1, Nil,
        If(
          Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
          If(
            Select(Ident(iter), "isEmpty"),
            EmptyTree,
            Block(
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
              Apply(Select(Ident(iter), "step"), Nil) :: Nil,
              Apply(Ident(loop1), Nil))),
          EmptyTree)) ::
      LabelDef(loop2, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            Apply(Select(Ident(b), "$plus$eq"), Select(Ident(iter), "head") :: Nil) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop2), Nil)))) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def take[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, upper: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val b    = BufferName(c)(buffer)
    val i    = c.fresh(newTermName("i$"))
    val n    = c.fresh(newTermName("n$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(buffer, b) ::
      ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
      ValDef(NoMods, n, TypeTree(), upper) ::
      LabelDef(loop, Nil,
        If(
          Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
          If(
            Select(Ident(iter), "isEmpty"),
            EmptyTree,
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Select(Ident(iter), "head") :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
              Apply(Select(Ident(iter), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil))),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def slice[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, lower: c.Tree, upper: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter  = c.fresh(newTermName("iter$"))
    val b     = BufferName(c)(buffer)
    val i     = c.fresh(newTermName("i$"))
    val n     = c.fresh(newTermName("n$"))
    val loop1 = c.fresh(newTermName("loop$"))
    val loop2 = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      BufferDef(c)(buffer, b) ::
      ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
      ValDef(Modifiers(Flag.MUTABLE), n, TypeTree(), lower) ::
      LabelDef(loop1, Nil,
        If(
          Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
          If(
            Select(Ident(iter), "isEmpty"),
            EmptyTree,
            Block(
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
              Apply(Select(Ident(iter), "step"), Nil) :: Nil,
              Apply(Ident(loop1), Nil))),
          EmptyTree)) ::
      Assign(Ident(n), upper) ::
      LabelDef(loop2, Nil,
        If(
          Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
          If(
            Select(Ident(iter), "isEmpty"),
            EmptyTree,
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Select(Ident(iter), "head") :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
              Apply(Select(Ident(iter), "step"), Nil) :: Nil,
              Apply(Ident(loop2), Nil))),
          EmptyTree)) :: Nil,
      Select(Ident(b), "state"))
  }
  
  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(xs: c.Tree, ys: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val iter1 = c.fresh(newTermName("iter$"))
    val iter2 = c.fresh(newTermName("iter$"))
    val b     = BufferName(c)(buffer)
    val loop  = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter1, IteratorTpe[A](c), xs) ::
      ValDef(NoMods, iter2, IteratorTpe[B](c), ys) ::
      BufferDef(c)(buffer, b) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter1), "isEmpty"),
          EmptyTree,
          If(
            Select(Ident(iter2), "isEmpty"),
            EmptyTree,
            Block(
              Apply(Select(Ident(b), "$plus$eq"),
                    NewTuple2(c)(Select(Ident(iter1), "head"), Select(Ident(iter2), "head")) :: Nil) ::
              Apply(Select(Ident(iter1), "step"), Nil) ::
              Apply(Select(Ident(iter2), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil))))) :: Nil,
      Select(Ident(b), "result"))
  }
  
  def ++ [A : c.WeakTypeTag, B >: A : c.WeakTypeTag](c: Context)(xs: c.Tree, ys: c.Tree, buffer: c.Tree): c.Tree = {
    import c.universe._
    val b = BufferName(c)(buffer)
    Block(
      BufferDef(c)(buffer, b) ::
      Apply(Select(Ident(b), "$plus$plus$eq"), xs :: Nil) ::
      Apply(Select(Ident(b), "$plus$plus$eq"), ys :: Nil) :: Nil,
      Select(Ident(b), "result"))
  }
  
  private def IteratorTpe[A : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    AppliedTypeTree(
      Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "collections"), newTypeName("Iterator")),
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
  
  private def NewTuple2(c: Context)(_1: c.Tree, _2: c.Tree): c.Tree = {
    import c.universe._
    New(
      Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
      (_1 :: _2 :: Nil) :: Nil)
  }
}
