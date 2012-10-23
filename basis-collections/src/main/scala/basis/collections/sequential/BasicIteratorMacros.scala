/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

private[sequential] object BasicIteratorMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def foreach[A : c.WeakTypeTag, U](c: Context)(iterator: c.Tree, f: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) :: Nil,
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            Apply(f, Select(Ident(iter), "head") :: Nil) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop), Nil)))))
  }
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(iterator: c.Tree, z: c.Tree, op: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), z) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            Assign(Ident(r), Apply(op, Ident(r) :: Select(Ident(iter), "head") :: Nil)) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop), Nil)))) :: Nil,
      Ident(r))
  }
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag](c: Context)(iterator: c.Tree, op: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      If(
        Select(Ident(iter), "isEmpty"),
        Throw(New(UnsupportedOperationExceptionTpe(c), (Literal(Constant("Empty reduce.")) :: Nil) :: Nil)),
        EmptyTree) ::
      ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(iter), "head")) ::
      Apply(Select(Ident(iter), "step"), Nil) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            Assign(Ident(r), Apply(op, Ident(r) :: Select(Ident(iter), "head") :: Nil)) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop), Nil)))) :: Nil,
      Ident(r))
  }
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag](c: Context)(iterator: c.Tree, op: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) :: Nil,
      If(
        Select(Ident(iter), "isEmpty"),
        SelectNone(c),
        Block(
          ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(iter), "head")) ::
          Apply(Select(Ident(iter), "step"), Nil) ::
          LabelDef(loop, Nil,
            If(
              Select(Ident(iter), "isEmpty"),
              EmptyTree,
              Block(
                Assign(Ident(r), Apply(op, Ident(r) :: Select(Ident(iter), "head") :: Nil)) ::
                Apply(Select(Ident(iter), "step"), Nil) :: Nil,
                Apply(Ident(loop), Nil)))) :: Nil,
          Apply(SelectSome(c), Ident(r) :: Nil))))
  }
  
  def find[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, p: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      ValDef(Modifiers(Flag.MUTABLE), r, OptionTpe[A](c), SelectNone(c)) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(iter), "head")) :: Nil,
            If(
              Apply(p, Ident(x) :: Nil),
              Assign(Ident(r), Apply(SelectSome(c), Ident(x) :: Nil)),
              Block(
                Apply(Select(Ident(iter), "step"), Nil) :: Nil,
                Apply(Ident(loop), Nil)))))) :: Nil,
      Ident(r))
  }
  
  def forall[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, p: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          If(
            Apply(p, Select(Ident(iter), "head") :: Nil),
            Block(
              Apply(Select(Ident(iter), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            Assign(Ident(r), Literal(Constant(false)))))) :: Nil,
      Ident(r))
  }
  
  def exists[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, p: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          If(
            Apply(p, Select(Ident(iter), "head") :: Nil),
            Assign(Ident(r), Literal(Constant(true))),
            Block(
              Apply(Select(Ident(iter), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil))))) :: Nil,
      Ident(r))
  }
  
  def count[A : c.WeakTypeTag](c: Context)(iterator: c.Tree, p: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val t    = c.fresh(newTermName("total$"))
    val loop = c.fresh(newTermName("loop$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            If(
              Apply(p, Select(Ident(iter), "head") :: Nil),
              Assign(Ident(t), Apply(Select(Ident(t), "$plus"), Literal(Constant(1)) :: Nil)),
              EmptyTree) ::
            Apply(Select(Ident(iter), "step"), Nil) :: Nil,
            Apply(Ident(loop), Nil)))) :: Nil,
      Ident(t))
  }
  
  def select[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(iterator: c.Tree, q: c.Tree): c.Tree = {
    import c.universe._
    val iter = c.fresh(newTermName("iter$"))
    val r    = c.fresh(newTermName("result$"))
    val loop = c.fresh(newTermName("loop$"))
    val x    = c.fresh(newTermName("head$"))
    Block(
      ValDef(NoMods, iter, IteratorTpe[A](c), iterator) ::
      ValDef(Modifiers(Flag.MUTABLE), r, OptionTpe[B](c), SelectNone(c)) ::
      LabelDef(loop, Nil,
        If(
          Select(Ident(iter), "isEmpty"),
          EmptyTree,
          Block(
            ValDef(NoMods, x, TypeTree(weakTypeOf[A]), Select(Ident(iter), "head")) :: Nil,
            If(
              Apply(Select(q, "isDefinedAt"), Ident(x) :: Nil),
              Assign(Ident(r), Apply(SelectSome(c), Apply(q, Ident(x) :: Nil) :: Nil)),
              Block(
                Apply(Select(Ident(iter), "step"), Nil) :: Nil,
                Apply(Ident(loop), Nil)))))) :: Nil,
      Ident(r))
  }
  
  private def IteratorTpe[A : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    AppliedTypeTree(
      Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "collections"), newTypeName("Iterator")),
      TypeTree(weakTypeOf[A]) :: Nil)
  }
  
  private def OptionTpe[A : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    AppliedTypeTree(
      Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Option")),
      TypeTree(weakTypeOf[A]) :: Nil)
  }
  
  private def SelectSome(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Ident(nme.ROOTPKG), "scala"), "Some")
  }
  
  private def SelectNone(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Ident(nme.ROOTPKG), "scala"), "None")
  }
  
  private def UnsupportedOperationExceptionTpe(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), newTypeName("UnsupportedOperationException"))
  }
}
