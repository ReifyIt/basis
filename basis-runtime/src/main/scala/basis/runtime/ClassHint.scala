/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

import scala.annotation.implicitNotFound

/** Run-time type information about a class.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  */
@implicitNotFound("No available ClassHint for ${T}.")
trait ClassHint[T] extends Equals with TypeHint[T] {
  def runtimeClass: java.lang.Class[_]
  
  def newArray(length: Int): Array[T] = {
    val clazz = runtimeClass
    if      (clazz eq java.lang.Byte.TYPE)      new Array[Byte](length).asInstanceOf[Array[T]]
    else if (clazz eq java.lang.Short.TYPE)     new Array[Short](length).asInstanceOf[Array[T]]
    else if (clazz eq java.lang.Character.TYPE) new Array[Char](length).asInstanceOf[Array[T]]
    else if (clazz eq java.lang.Integer.TYPE)   new Array[Int](length).asInstanceOf[Array[T]]
    else if (clazz eq java.lang.Long.TYPE)      new Array[Long](length).asInstanceOf[Array[T]]
    else if (clazz eq java.lang.Float.TYPE)     new Array[Float](length).asInstanceOf[Array[T]]
    else if (clazz eq java.lang.Double.TYPE)    new Array[Double](length).asInstanceOf[Array[T]]
    else if (clazz eq java.lang.Boolean.TYPE)   new Array[Boolean](length).asInstanceOf[Array[T]]
    else if (clazz eq java.lang.Void.TYPE)      new Array[Unit](length).asInstanceOf[Array[T]]
    else java.lang.reflect.Array.newInstance(clazz, length).asInstanceOf[Array[T]]
  }
  
  override def canEqual(other: Any): Boolean = other.isInstanceOf[ClassHint[_]]
  
  override def equals(other: Any): Boolean = other match {
    case that: ClassHint[_] => runtimeClass.equals(that.runtimeClass)
    case _ => false
  }
  
  override def hashCode: Int = runtimeClass.##
  
  override def toString: String = {
    if (!runtimeClass.isArray) runtimeClass.getName
    else "Array"+"["+ ClassHint(runtimeClass.getComponentType) +"]"
  }
}

/** A factory for [[ClassHint class hints]]. */
object ClassHint {
  import Predef.classOf
  
  def apply[T](runtimeClass: java.lang.Class[_]): ClassHint[T] = {
    if      (runtimeClass eq java.lang.Byte.TYPE)       TypeHint.Byte.asInstanceOf[ClassHint[T]]
    else if (runtimeClass eq java.lang.Short.TYPE)      TypeHint.Short.asInstanceOf[ClassHint[T]]
    else if (runtimeClass eq java.lang.Character.TYPE)  TypeHint.Char.asInstanceOf[ClassHint[T]]
    else if (runtimeClass eq java.lang.Integer.TYPE)    TypeHint.Int.asInstanceOf[ClassHint[T]]
    else if (runtimeClass eq java.lang.Long.TYPE)       TypeHint.Long.asInstanceOf[ClassHint[T]]
    else if (runtimeClass eq java.lang.Float.TYPE)      TypeHint.Float.asInstanceOf[ClassHint[T]]
    else if (runtimeClass eq java.lang.Double.TYPE)     TypeHint.Double.asInstanceOf[ClassHint[T]]
    else if (runtimeClass eq java.lang.Boolean.TYPE)    TypeHint.Boolean.asInstanceOf[ClassHint[T]]
    else if (runtimeClass eq java.lang.Void.TYPE)       TypeHint.Unit.asInstanceOf[ClassHint[T]]
    else if (runtimeClass eq classOf[java.lang.Object]) TypeHint.Any.asInstanceOf[ClassHint[T]]
    else new RuntimeClassHint(runtimeClass)
  }
  
  def Array[T](implicit T: ClassHint[T]): ClassHint[Array[T]] = new RuntimeClassHint(T.newArray(0).getClass)
}

private[runtime] final class RuntimeClassHint[T](override val runtimeClass: java.lang.Class[_]) extends ClassHint[T]
