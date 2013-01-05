/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

import scala.annotation.implicitNotFound

/** A typeclass for optional run-time type information.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  */
@implicitNotFound("No available TypeHint for ${T}.")
trait TypeHint[T]

/** A factory for builtin [[TypeHint type hints]]. */
object TypeHint {
  import Predef.classOf
  
  def apply[T](implicit T: TypeHint[T]): T.type = T
  
  implicit object Any extends ClassHint[Any] {
    override def runtimeClass: java.lang.Class[_] = classOf[java.lang.Object]
    override def toString: String = "Any"
  }
  
  implicit object AnyVal extends ClassHint[AnyVal] {
    override def runtimeClass: java.lang.Class[_] = classOf[java.lang.Object]
    override def toString: String = "AnyVal"
  }
  
  implicit object Byte extends ClassHint[Byte] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Byte.TYPE
    override def newArray(length: Int): Array[Byte] = new Array[Byte](length)
    override def toString: String = "Byte"
  }
  
  implicit object Short extends ClassHint[Short] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Short.TYPE
    override def newArray(length: Int): Array[Short] = new Array[Short](length)
    override def toString: String = "Short"
  }
  
  implicit object Char extends ClassHint[Char] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Character.TYPE
    override def newArray(length: Int): Array[Char] = new Array[Char](length)
    override def toString: String = "Char"
  }
  
  implicit object Int extends ClassHint[Int] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Integer.TYPE
    override def newArray(length: Int): Array[Int] = new Array[Int](length)
    override def toString: String = "Int"
  }
  
  implicit object Long extends ClassHint[Long] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Long.TYPE
    override def newArray(length: Int): Array[Long] = new Array[Long](length)
    override def toString: String = "Long"
  }
  
  implicit object Float extends ClassHint[Float] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Float.TYPE
    override def newArray(length: Int): Array[Float] = new Array[Float](length)
    override def toString: String = "Float"
  }
  
  implicit object Double extends ClassHint[Double] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Double.TYPE
    override def newArray(length: Int): Array[Double] = new Array[Double](length)
    override def toString: String = "Double"
  }
  
  implicit object Boolean extends ClassHint[Boolean] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Boolean.TYPE
    override def newArray(length: Int): Array[Boolean] = new Array[Boolean](length)
    override def toString: String = "Boolean"
  }
  
  implicit object Unit extends ClassHint[Unit] {
    override def runtimeClass: java.lang.Class[_] = java.lang.Void.TYPE
    override def newArray(length: Int): Array[Unit] = new Array[Unit](length)
    override def toString: String = "Unit"
  }
  
  implicit object AnyRef extends ClassHint[AnyRef] {
    override def runtimeClass: java.lang.Class[_] = classOf[java.lang.Object]
    override def newArray(length: Int): Array[AnyRef] = new Array[AnyRef](length)
    override def toString: String = "AnyRef"
  }
  
  implicit object Nothing extends ClassHint[Nothing] {
    override def runtimeClass: java.lang.Class[_] = classOf[scala.runtime.Nothing$]
    override def newArray(length: Int): Array[Nothing] = new Array[Nothing](length)
    override def toString: String = "Nothing"
  }
  
  implicit def Array[T](implicit T: ClassHint[T]): ClassHint[Array[T]] = ClassHint.Array(T)
  
  implicit def Undefined[T]: TypeHint[T] = NoTypeHint.asInstanceOf[TypeHint[T]]
}

private[runtime] object NoTypeHint extends TypeHint[Any] {
  override def toString: String = "NoTypeHint"
}
