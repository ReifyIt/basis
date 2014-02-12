package basis
package util

import scala.reflect.{ ClassTag, classTag }
import scala.reflect.macros.Context

trait Types {
  def applied[M](c: Context)(implicit tag: c.WeakTypeTag[M]): c.WeakTypeTag[M] = tag

  def applied[M[X1], A1](c: Context)(implicit ctag: ClassTag[M[_]], a1: c.WeakTypeTag[A1]): c.WeakTypeTag[M[A1]] = {
    import c.universe.{ appliedType, weakTypeOf }
    c.WeakTypeTag[M[A1]](appliedType(c.mirror.staticClass(ctag.runtimeClass.getName).toType.typeConstructor, weakTypeOf[A1] :: Nil))
  }
  def applied[M[X1, X2], A1, A2](c: Context)(implicit ctag: ClassTag[M[_, _]], a1: c.WeakTypeTag[A1], a2: c.WeakTypeTag[A2]): c.WeakTypeTag[M[A1, A2]] = {
    import c.universe.{ appliedType, weakTypeOf }
    c.WeakTypeTag[M[A1, A2]](appliedType(c.mirror.staticClass(ctag.runtimeClass.getName).toType.typeConstructor, weakTypeOf[A1] :: weakTypeOf[A2] :: Nil))
  }

  def staticType[T](c: Context)(implicit tag: ClassTag[T]): c.universe.Type     = c.mirror.staticClass(tag.runtimeClass.getName).toType
  def staticTypeTag[T](c: Context)(implicit tag: ClassTag[T]): c.WeakTypeTag[T] = c.WeakTypeTag[T](staticType[T](c))
}
