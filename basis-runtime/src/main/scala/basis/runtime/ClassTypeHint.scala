/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

import scala.annotation.implicitNotFound

/** Runtime type information about a class. */
@implicitNotFound("No available ClassTypeHint for ${T}.")
trait ClassTypeHint[T] extends Equals with TypeHint[T] {
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
    else java.lang.reflect.Array.newInstance(runtimeClass, length).asInstanceOf[Array[T]]
  }
  
  override def canEqual(other: Any): Boolean = other.isInstanceOf[ClassTypeHint[_]]
  
  override def equals(other: Any): Boolean = other match {
    case that: ClassTypeHint[_] => runtimeClass.equals(that.runtimeClass)
    case _ => false
  }
  
  override def hashCode: Int = runtimeClass.##
  
  override def toString: String = {
    if (!runtimeClass.isArray) runtimeClass.getName
    else "Array"+"["+ TypeHint(runtimeClass.getComponentType) +"]"
  }
}

private[runtime] final class RuntimeClassTypeHint[T]
    (override val runtimeClass: java.lang.Class[_])
  extends ClassTypeHint[T]
