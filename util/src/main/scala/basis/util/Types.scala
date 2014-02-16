package basis
package util

import scala.reflect.{ ClassTag, classTag }
import scala.reflect.macros.Context

trait Types {
  type ContextWithPre[+T] = Context { type PrefixType <: T }

  private def typesApplied(c: Context)(types: c.Type*): c.universe.Type = {
    import c.universe._
    val tcon :: args = types.toList
    appliedType(tcon.typeConstructor, args)
  }

  def applied[M](c: Context)(implicit tag: c.WeakTypeTag[M]): c.WeakTypeTag[M] = tag

  def applied[M[X1], A1](c: Context)(implicit tcon: c.WeakTypeTag[M[_]], a1: c.WeakTypeTag[A1]): c.WeakTypeTag[M[A1]] = {
    import c.universe.weakTypeOf
    c.WeakTypeTag[M[A1]](typesApplied(c)(weakTypeOf[M[_]], weakTypeOf[A1]))
  }
  def applied[M[X1, X2], A1, A2](c: Context)(implicit tcon: c.WeakTypeTag[M[_, _]], a1: c.WeakTypeTag[A1], a2: c.WeakTypeTag[A2]): c.WeakTypeTag[M[A1, A2]] = {
    import c.universe.weakTypeOf
    c.WeakTypeTag[M[A1, A2]](typesApplied(c)(weakTypeOf[M[_, _]], weakTypeOf[A1], weakTypeOf[A2]))
  }

  def depType[T](c: Context)(outer: c.Expr[T], name: String): c.universe.Type = {
    import c.universe._
    typeRef(outer.staticType, outer.staticType.member(name: TypeName), Nil).normalize
  }

  def staticType[T](c: Context)(implicit tag: ClassTag[T]): c.universe.Type     = c.mirror.staticClass(tag.runtimeClass.getName).toType
  def staticTypeTag[T](c: Context)(implicit tag: ClassTag[T]): c.WeakTypeTag[T] = c.WeakTypeTag[T](staticType[T](c))
}
