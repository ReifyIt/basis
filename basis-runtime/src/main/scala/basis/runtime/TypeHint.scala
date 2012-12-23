/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

import scala.annotation.implicitNotFound

/** A typeclass for optional runtime type information. */
@implicitNotFound("No available TypeHint for ${T}.")
trait TypeHint[T]

/** A factory for builtin implicit type hints. */
object TypeHint {
  import Predef.classOf
  
  def apply[T](implicit T: TypeHint[T]): T.type = T
  
  implicit object Any extends ClassTypeHint[Any] {
    override def runtimeClass: java.lang.Class[_] = classOf[java.lang.Object]
    override def toString: String = "Any"
  }
  
  implicit object AnyVal extends ClassTypeHint[AnyVal] {
    override def runtimeClass: java.lang.Class[_] = classOf[java.lang.Object]
    override def toString: String = "AnyVal"
  }
  
  implicit object Byte extends ClassTypeHint[Byte] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Byte.TYPE
    override def newArray(length: Int): Array[Byte] = new Array[Byte](length)
    override def toString: String = "Byte"
  }
  
  implicit object Short extends ClassTypeHint[Short] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Short.TYPE
    override def newArray(length: Int): Array[Short] = new Array[Short](length)
    override def toString: String = "Short"
  }
  
  implicit object Char extends ClassTypeHint[Char] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Character.TYPE
    override def newArray(length: Int): Array[Char] = new Array[Char](length)
    override def toString: String = "Char"
  }
  
  implicit object Int extends ClassTypeHint[Int] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Integer.TYPE
    override def newArray(length: Int): Array[Int] = new Array[Int](length)
    override def toString: String = "Int"
  }
  
  implicit object Long extends ClassTypeHint[Long] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Long.TYPE
    override def newArray(length: Int): Array[Long] = new Array[Long](length)
    override def toString: String = "Long"
  }
  
  implicit object Float extends ClassTypeHint[Float] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Float.TYPE
    override def newArray(length: Int): Array[Float] = new Array[Float](length)
    override def toString: String = "Float"
  }
  
  implicit object Double extends ClassTypeHint[Double] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Double.TYPE
    override def newArray(length: Int): Array[Double] = new Array[Double](length)
    override def toString: String = "Double"
  }
  
  implicit object Boolean extends ClassTypeHint[Boolean] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Boolean.TYPE
    override def newArray(length: Int): Array[Boolean] = new Array[Boolean](length)
    override def toString: String = "Boolean"
  }
  
  implicit object Unit extends ClassTypeHint[Unit] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Void.TYPE
    override def newArray(length: Int): Array[Unit] = new Array[Unit](length)
    override def toString: String = "Unit"
  }
  
  implicit object AnyRef extends ClassTypeHint[AnyRef] {
    override def runtimeClass: java.lang.Class[_] = classOf[java.lang.Object]
    override def newArray(length: Int): Array[AnyRef] = new Array[AnyRef](length)
    override def toString: String = "AnyRef"
  }
  
  implicit object Nothing extends ClassTypeHint[Nothing] {
    override def runtimeClass: java.lang.Class[_] = classOf[scala.runtime.Nothing$]
    override def newArray(length: Int): Array[Nothing] = new Array[Nothing](length)
    override def toString: String = "Nothing"
  }
  
  implicit def Array[T](implicit T: ClassTypeHint[T]): ClassTypeHint[Array[T]] = ClassTypeHint.Array(T)
  
  implicit def Undefined[T]: TypeHint[T] = NoTypeHint.asInstanceOf[TypeHint[T]]
}

private[runtime] object NoTypeHint extends TypeHint[Any] {
  override def toString: String = "NoTypeHint"
}
