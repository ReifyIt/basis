/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

trait SeqFactory[CC[A] <: Seq[A]] {
  def apply[A](xs: A*)(implicit buffer: Buffer[CC[_], A]): buffer.State =
    macro SeqFactory.apply[A]
  
  def fill[A](count: Int)(element: => A)(implicit buffer: Buffer[CC[_], A]): buffer.State =
    macro SeqFactory.fill[A]
  
  def tabulate[A](count: Int)(f: Int => A)(implicit buffer: Buffer[CC[_], A]): buffer.State =
    macro SeqFactory.tabulate[A]
  
  def iterate[A](start: A, count: Int)(f: A => A)(implicit buffer: Buffer[CC[_], A]): buffer.State =
    macro SeqFactory.iterate[A]
}

private object SeqFactory {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[A](c: Context)(xs: c.Expr[A]*)(buffer: c.Expr[Buffer[_, A]]): c.Expr[buffer.value.State] = {
    import c.universe._
    var b = Apply(Select(buffer.tree, "expect"), Literal(Constant(xs.length)) :: Nil)
    val iter = xs.iterator
    while (iter.hasNext) b = Apply(Select(b, "$plus$eq"), iter.next().tree :: Nil)
    c.Expr(Select(b, "state"))(TypeTag.Nothing)
  }
  
  def fill[A]
      (c: Context)
      (count: c.Expr[Int])
      (element: c.Expr[A])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] = {
    import c.universe._
    val i    = c.fresh(newTermName("i$"))
    val b    = BufferName(c)(buffer.tree)
    val loop = c.fresh(newTermName("loop$"))
    c.Expr(
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), count.tree) ::
        BufferDefExpect(c)(buffer.tree, b, Ident(i)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$greater"), Literal(Constant(0)) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), element.tree :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))(TypeTag.Nothing)
  }
  
  def tabulate[A]
      (c: Context)
      (count: c.Expr[Int])
      (f: c.Expr[Int => A])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] = {
    import c.universe._
    val i    = c.fresh(newTermName("i$"))
    val n    = c.fresh(newTermName("n$"))
    val b    = BufferName(c)(buffer.tree)
    val loop = c.fresh(newTermName("loop$"))
    c.Expr(
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        BufferDefExpect(c)(buffer.tree, b, Ident(n)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(f.tree, Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))(TypeTag.Nothing)
  }
  
  def iterate[A]
      (c: Context)
      (start: c.Expr[A], count: c.Expr[Int])
      (f: c.Expr[A => A])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] = {
    import c.universe._
    val n    = c.fresh(newTermName("n$"))
    val b    = BufferName(c)(buffer.tree)
    val a    = c.fresh(newTermName("a$"))
    val i    = c.fresh(newTermName("i$"))
    val loop = c.fresh(newTermName("loop$"))
    c.Expr(
      Block(
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        BufferDefExpect(c)(buffer.tree, b, Ident(n)) ::
        If(
          Apply(Select(Ident(n), "$greater"), Literal(Constant(0)) :: Nil),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), a, TypeTree(), start.tree) ::
            Apply(Select(Ident(b), "$plus$eq"), Ident(a) :: Nil) ::
            ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) :: Nil,
            LabelDef(loop, Nil,
              If(
                Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
                Block(
                  Assign(Ident(a), Apply(f.tree, Ident(a) :: Nil)) ::
                  Apply(Select(Ident(b), "$plus$eq"), Ident(a) :: Nil) ::
                  Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree))),
          EmptyTree) :: Nil,
        Select(Ident(b), "state")))(TypeTag.Nothing)
  }
  
  private def BufferName(c: Context)(buffer: c.Tree): c.TermName = {
    import c.universe._
    buffer match {
      case Ident(stable) => stable.toTermName
      case _ => c.fresh(newTermName("buffer$"))
    }
  }
  
  private def BufferDefExpect(c: Context)(buffer: c.Tree, name: c.TermName, count: c.Tree): c.Tree = {
    import c.universe._
    buffer match {
      case Ident(_) => Apply(Select(buffer, "expect"), count :: Nil)
      case _ => ValDef(NoMods, name, TypeTree(), Apply(Select(buffer, "expect"), count :: Nil))
    }
  }
}
