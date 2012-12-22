/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

/** Runtime type information about a class. */
class ClassTypeHint[T](val runtimeClass: java.lang.Class[_]) extends TypeHint[T] {
  import Predef.<:<
  
  def Array: ClassTypeHint[Array[T]] = new ClassTypeHint(newArray(0).getClass)
  
  def unArray[A](implicit isArray: T <:< Array[A]): ClassTypeHint[A] = ClassTypeHint(runtimeClass.getComponentType)
  
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
  
  override def equals(other: Any): Boolean = other match {
    case that: ClassTypeHint[_] => runtimeClass.equals(that.runtimeClass)
    case _ => false
  }
  
  override def hashCode: Int = runtimeClass.##
  
  override def toString: String = {
    if (!runtimeClass.isArray) runtimeClass.getName
    else "Array"+"["+ ClassTypeHint(runtimeClass.getComponentType) +"]"
  }
}

/** A factory for class type hints. */
object ClassTypeHint {
  import Predef.classOf
  
  def apply[T](runtimeClass: java.lang.Class[_]): ClassTypeHint[T] = {
    if      (runtimeClass eq java.lang.Byte.TYPE)             ClassTypeHint.Byte.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq java.lang.Short.TYPE)            ClassTypeHint.Short.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq java.lang.Character.TYPE)        ClassTypeHint.Char.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq java.lang.Integer.TYPE)          ClassTypeHint.Int.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq java.lang.Long.TYPE)             ClassTypeHint.Long.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq java.lang.Float.TYPE)            ClassTypeHint.Float.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq java.lang.Double.TYPE)           ClassTypeHint.Double.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq java.lang.Boolean.TYPE)          ClassTypeHint.Boolean.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq java.lang.Void.TYPE)             ClassTypeHint.Unit.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq classOf[java.lang.Object])       ClassTypeHint.Any.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq classOf[scala.runtime.Null$])    ClassTypeHint.Null.asInstanceOf[ClassTypeHint[T]]
    else if (runtimeClass eq classOf[scala.runtime.Nothing$]) ClassTypeHint.Nothing.asInstanceOf[ClassTypeHint[T]]
    else new ClassTypeHint(runtimeClass)
  }
  
  private[runtime] object Any extends ClassTypeHint[Any](classOf[java.lang.Object]) {
    override def toString: String = "Any"
  }
  
  private[runtime] object AnyVal extends ClassTypeHint[AnyVal](classOf[java.lang.Object]) {
    override def toString: String = "AnyVal"
  }
  
  private[runtime] object Byte extends ClassTypeHint[Byte](java.lang.Byte.TYPE) {
    override def newArray(length: Int): Array[Byte] = new Array[Byte](length)
    override def toString: String = "Byte"
  }
  
  private[runtime] object Short extends ClassTypeHint[Short](java.lang.Short.TYPE) {
    override def newArray(length: Int): Array[Short] = new Array[Short](length)
    override def toString: String = "Short"
  }
  
  private[runtime] object Char extends ClassTypeHint[Char](java.lang.Character.TYPE) {
    override def newArray(length: Int): Array[Char] = new Array[Char](length)
    override def toString: String = "Char"
  }
  
  private[runtime] object Int extends ClassTypeHint[Int](java.lang.Integer.TYPE) {
    override def newArray(length: Int): Array[Int] = new Array[Int](length)
    override def toString: String = "Int"
  }
  
  private[runtime] object Long extends ClassTypeHint[Long](java.lang.Long.TYPE) {
    override def newArray(length: Int): Array[Long] = new Array[Long](length)
    override def toString: String = "Long"
  }
  
  private[runtime] object Float extends ClassTypeHint[Float](java.lang.Float.TYPE) {
    override def newArray(length: Int): Array[Float] = new Array[Float](length)
    override def toString: String = "Float"
  }
  
  private[runtime] object Double extends ClassTypeHint[Double](java.lang.Double.TYPE) {
    override def newArray(length: Int): Array[Double] = new Array[Double](length)
    override def toString: String = "Double"
  }
  
  private[runtime] object Boolean extends ClassTypeHint[Boolean](java.lang.Boolean.TYPE) {
    override def newArray(length: Int): Array[Boolean] = new Array[Boolean](length)
    override def toString: String = "Boolean"
  }
  
  private[runtime] object Unit extends ClassTypeHint[Unit](java.lang.Void.TYPE) {
    override def newArray(length: Int): Array[Unit] = new Array[Unit](length)
    override def toString: String = "Unit"
  }
  
  private[runtime] object AnyRef extends ClassTypeHint[AnyRef](classOf[java.lang.Object]) {
    override def newArray(length: Int): Array[AnyRef] = new Array[AnyRef](length)
    override def toString: String = "AnyRef"
  }
  
  private[runtime] object Null extends ClassTypeHint[Null](classOf[scala.runtime.Null$]) {
    override def newArray(length: Int): Array[Null] = new Array[Null](length)
    override def toString: String = "Null"
  }
  
  private[runtime] object Nothing extends ClassTypeHint[Nothing](classOf[scala.runtime.Nothing$]) {
    override def newArray(length: Int): Array[Nothing] = new Array[Nothing](length)
    override def toString: String = "Nothing"
  }
}
