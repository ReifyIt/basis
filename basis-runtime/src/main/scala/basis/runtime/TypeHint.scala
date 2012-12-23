/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

/** A typeclass for optional runtime type information. */
trait TypeHint[T]

/** A factory for builtin implicit type hints. */
object TypeHint {
  import Predef.classOf
  
  def apply[T](implicit T: TypeHint[T]): T.type = T
  
  implicit def omit[T]: TypeHint[T] = NoTypeHint.asInstanceOf[TypeHint[T]]
  
  implicit val Any: ClassTypeHint[Any] = ClassTypeHint.Any
  
  implicit val AnyVal: ClassTypeHint[AnyVal] = ClassTypeHint.AnyVal
  
  implicit val Byte: ClassTypeHint[Byte] = ClassTypeHint.Byte
  
  implicit val Short: ClassTypeHint[Short] = ClassTypeHint.Short
  
  implicit val Char: ClassTypeHint[Char] = ClassTypeHint.Char
  
  implicit val Int: ClassTypeHint[Int] = ClassTypeHint.Int
  
  implicit val Long: ClassTypeHint[Long] = ClassTypeHint.Long
  
  implicit val Float: ClassTypeHint[Float] = ClassTypeHint.Float
  
  implicit val Double: ClassTypeHint[Double] = ClassTypeHint.Double
  
  implicit val Boolean: ClassTypeHint[Boolean] = ClassTypeHint.Boolean
  
  implicit val Unit: ClassTypeHint[Unit] = ClassTypeHint.Unit
  
  implicit val AnyRef: ClassTypeHint[AnyRef] = ClassTypeHint.AnyRef
  
  implicit val Null: ClassTypeHint[Null] = ClassTypeHint.Null
  
  implicit val Nothing: ClassTypeHint[Nothing] = ClassTypeHint.Nothing
  
  implicit def Array[T](implicit T: ClassTypeHint[T]): ClassTypeHint[Array[T]] = T.Array
}

private[runtime] object NoTypeHint extends TypeHint[Any] {
  override def toString: String = "NoTypeHint"
}
