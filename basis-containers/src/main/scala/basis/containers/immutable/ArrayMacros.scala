/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._

private[immutable] object ArrayMacros {
  import scala.collection.breakOut
  import scala.collection.immutable.{List, ::, Nil}
  import scala.reflect.macros.Context
  
  private def initArray(c: Context)(tpt: c.Tree, xs: List[c.Tree], length: Int): c.Tree = {
    import c.universe._
    val array = c.fresh(newTermName("array$"))
    Block(
      ValDef(Modifiers(), array, TypeTree(),
        Apply(
          Select(
            New(AppliedTypeTree(Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Array")), tpt :: Nil)),
            nme.CONSTRUCTOR),
          Literal(Constant(length)) :: Nil)) ::
      xs.zipWithIndex.map {
        case (x, i) => Apply(Select(Ident(array), "update"), Literal(Constant(i)) :: x :: Nil)
      },
      Ident(array)
    )
  }
  
  private def newByteArray(c: Context)(array: c.Tree): c.Tree = {
    import c.universe._
    Apply(
      Select(
        New(Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "immutable"), newTypeName("ByteArray"))),
        nme.CONSTRUCTOR),
      array :: Nil)
  }
  
  private def newShortArray(c: Context)(array: c.Tree): c.Tree = {
    import c.universe._
    Apply(
      Select(
        New(Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "immutable"), newTypeName("ShortArray"))),
        nme.CONSTRUCTOR),
      array :: Nil)
  }
  
  private def newIntArray(c: Context)(array: c.Tree): c.Tree = {
    import c.universe._
    Apply(
      Select(
        New(Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "immutable"), newTypeName("IntArray"))),
        nme.CONSTRUCTOR),
      array :: Nil)
  }
  
  private def newLongArray(c: Context)(array: c.Tree): c.Tree = {
    import c.universe._
    Apply(
      Select(
        New(Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "immutable"), newTypeName("LongArray"))),
        nme.CONSTRUCTOR),
      array :: Nil)
  }
  
  private def newFloatArray(c: Context)(array: c.Tree): c.Tree = {
    import c.universe._
    Apply(
      Select(
        New(Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "immutable"), newTypeName("FloatArray"))),
        nme.CONSTRUCTOR),
      array :: Nil)
  }
  
  private def newDoubleArray(c: Context)(array: c.Tree): c.Tree = {
    import c.universe._
    Apply(
      Select(
        New(Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "immutable"), newTypeName("DoubleArray"))),
        nme.CONSTRUCTOR),
      array :: Nil)
  }
  
  private def newRefArray(c: Context)(array: c.Tree): c.Tree = {
    import c.universe._
    Apply(
      Select(
        New(Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "immutable"), newTypeName("RefArray"))),
        nme.CONSTRUCTOR),
      array :: Nil)
  }
  
  def literalByteArray(c: Context)(xs: c.Expr[Byte]*): c.Expr[ByteArray] = {
    import c.universe._
    c.Expr(
      newByteArray(c)(
        initArray(c)(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Byte")),
          exprsToTrees(c)(xs: _*),
          xs.length))
    )(WeakTypeTag.Nothing)
  }
  
  def literalShortArray(c: Context)(xs: c.Expr[Short]*): c.Expr[ShortArray] = {
    import c.universe._
    c.Expr(
      newShortArray(c)(
        initArray(c)(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Short")),
          exprsToTrees(c)(xs: _*),
          xs.length))
    )(WeakTypeTag.Nothing)
  }
  
  def literalIntArray(c: Context)(xs: c.Expr[Int]*): c.Expr[IntArray] = {
    import c.universe._
    c.Expr(
      newIntArray(c)(
        initArray(c)(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Int")),
          exprsToTrees(c)(xs: _*),
          xs.length))
    )(WeakTypeTag.Nothing)
  }
  
  def literalLongArray(c: Context)(xs: c.Expr[Long]*): c.Expr[LongArray] = {
    import c.universe._
    c.Expr(
      newLongArray(c)(
        initArray(c)(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Long")),
          exprsToTrees(c)(xs: _*),
          xs.length))
    )(WeakTypeTag.Nothing)
  }
  
  def literalFloatArray(c: Context)(xs: c.Expr[Float]*): c.Expr[FloatArray] = {
    import c.universe._
    c.Expr(
      newFloatArray(c)(
        initArray(c)(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Float")),
          exprsToTrees(c)(xs: _*),
          xs.length))
    )(WeakTypeTag.Nothing)
  }
  
  def literalDoubleArray(c: Context)(xs: c.Expr[Double]*): c.Expr[DoubleArray] = {
    import c.universe._
    c.Expr(
      newDoubleArray(c)(
        initArray(c)(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Double")),
          exprsToTrees(c)(xs: _*),
          xs.length))
    )(WeakTypeTag.Nothing)
  }
  
  def literalRefArray[A : c.WeakTypeTag](c: Context)(xs: c.Expr[A]*): c.Expr[RefArray[A]] = {
    import c.universe._
    c.Expr[RefArray[A]](
      newRefArray(c)(
        initArray(c)(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("AnyRef")),
          exprsToAnyRefTrees(c)(xs: _*),
          xs.length))
    )
  }
  
  private def exprsToTrees(c: Context)(xs: c.Expr[_]*): List[c.Tree] = xs.map(_.tree)(breakOut)
  
  private def exprsToAnyRefTrees(c: Context)(xs: c.Expr[_]*): List[c.Tree] = {
    import c.universe._
    xs.map { x =>
      TypeApply(
        Select(x.tree, "asInstanceOf"),
        Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("AnyRef")) :: Nil)
    } (breakOut)
  }
}
