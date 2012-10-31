/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

private[immutable] object ArrayMacros {
  import scala.collection.immutable.{List, ::, Nil}
  import scala.reflect.macros.Context
  
  private def NewArray[A : c.WeakTypeTag](c: Context)(elems: List[c.Expr[A]]): c.Expr[scala.Array[A]] = {
    import c.{Expr, fresh, mirror, WeakTypeTag}
    import c.universe._
    val ArrayType = appliedType(mirror.staticClass("scala.Array").toType, weakTypeOf[A] :: Nil)
    val array = newTermName(fresh("array$"))
    val appends = List.newBuilder[Tree]
    var length = 0
    var xs = elems
    while (!xs.isEmpty) {
      appends += Apply(Select(Ident(array), "update"), Literal(Constant(length)) :: xs.head.tree :: Nil)
      length += 1
      xs = xs.tail
    }
    Expr {
      Block(
        ValDef(NoMods, array, TypeTree(), New(ArrayType, Literal(Constant(xs.length)))) ::
        appends.result,
        Ident(array))
    } (WeakTypeTag(ArrayType))
  }
  
  def ByteArray(c: Context)(xs: c.Expr[Byte]*): c.Expr[ByteArray] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val ArrayType = mirror.staticClass("basis.containers.immutable.ByteArray").toType
    Expr(New(ArrayType, NewArray(c)(xs.toList)(TypeTag.Byte).tree))(WeakTypeTag(ArrayType))
  }
  
  def ShortArray(c: Context)(xs: c.Expr[Short]*): c.Expr[ShortArray] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val ArrayType = mirror.staticClass("basis.containers.immutable.ShortArray").toType
    Expr(New(ArrayType, NewArray(c)(xs.toList)(TypeTag.Short).tree))(WeakTypeTag(ArrayType))
  }
  
  def IntArray(c: Context)(xs: c.Expr[Int]*): c.Expr[IntArray] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val ArrayType = mirror.staticClass("basis.containers.immutable.IntArray").toType
    Expr(New(ArrayType, NewArray(c)(xs.toList)(TypeTag.Int).tree))(WeakTypeTag(ArrayType))
  }
  
  def LongArray(c: Context)(xs: c.Expr[Long]*): c.Expr[LongArray] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val ArrayType = mirror.staticClass("basis.containers.immutable.LongArray").toType
    Expr(New(ArrayType, NewArray(c)(xs.toList)(TypeTag.Long).tree))(WeakTypeTag(ArrayType))
  }
  
  def FloatArray(c: Context)(xs: c.Expr[Float]*): c.Expr[FloatArray] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val ArrayType = mirror.staticClass("basis.containers.immutable.FloatArray").toType
    Expr(New(ArrayType, NewArray(c)(xs.toList)(TypeTag.Float).tree))(WeakTypeTag(ArrayType))
  }
  
  def DoubleArray(c: Context)(xs: c.Expr[Double]*): c.Expr[DoubleArray] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val ArrayType = mirror.staticClass("basis.containers.immutable.DoubleArray").toType
    Expr(New(ArrayType, NewArray(c)(xs.toList)(TypeTag.Double).tree))(WeakTypeTag(ArrayType))
  }
  
  def RefArray[A : c.WeakTypeTag](c: Context)(xs: c.Expr[A]*): c.Expr[RefArray[A]] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val ArrayType = appliedType(mirror.staticClass("basis.containers.immutable.RefArray").toType, weakTypeOf[A] :: Nil)
    val refs = List.newBuilder[Expr[AnyRef]]
    var ys = xs.toList
    while (!ys.isEmpty) {
      refs += Expr(TypeApply(Select(ys.head.tree, "asInstanceOf"), TypeTree(definitions.AnyRefTpe) :: Nil))(TypeTag.AnyRef)
      ys = ys.tail
    }
    Expr(New(ArrayType, NewArray(c)(refs.result)(TypeTag.AnyRef).tree))(WeakTypeTag(ArrayType))
  }
}
