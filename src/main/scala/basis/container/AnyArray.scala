/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis.collection._
import basis.memory._

trait AnyArray[A] extends Any with IndexedSeq[A] {
  override type Kind <: AnyArray[_]
  
  override def length: Int
  
  override def apply(index: Int): A
  
  def update(index: Int, value: A): Unit
  
  def copy(length: Int = this.length): AnyArray[A]
}

object AnyArray extends AllArrayBuilders {
  import ValType._
  
  def apply[A](length: Int)(implicit typeA: DataType[A]): AnyArray[A] = typeA match {
    case typeA: RefType[A]           => RefArray[A](length)
    case PackedByte                  => ByteArray(length).asInstanceOf[AnyArray[A]]
    case PackedShort  | PaddedShort  => ShortArray(length).asInstanceOf[AnyArray[A]]
    case PackedInt    | PaddedInt    => IntArray(length).asInstanceOf[AnyArray[A]]
    case PackedLong   | PaddedLong   => LongArray(length).asInstanceOf[AnyArray[A]]
    case PackedChar   | PaddedChar   => CharArray(length).asInstanceOf[AnyArray[A]]
    case PackedFloat  | PaddedFloat  => FloatArray(length).asInstanceOf[AnyArray[A]]
    case PackedDouble | PaddedDouble => DoubleArray(length).asInstanceOf[AnyArray[A]]
    case PackedBoolean               => BitArray(length).asInstanceOf[AnyArray[A]]
    case typeA: ValType[A]           => ValArray[A](length)(typeA)
  }
  
  def AnyArrayBuilder[A](implicit typeA: DataType[A]): Builder[Any, A] = typeA match {
    case typeA: RefType[A]           => RefArrayBuilder[A]
    case PackedByte                  => ByteArrayBuilder.asInstanceOf[Builder[Any, A]]
    case PackedShort  | PaddedShort  => ShortArrayBuilder.asInstanceOf[Builder[Any, A]]
    case PackedInt    | PaddedInt    => IntArrayBuilder.asInstanceOf[Builder[Any, A]]
    case PackedLong   | PaddedLong   => LongArrayBuilder.asInstanceOf[Builder[Any, A]]
    case PackedChar   | PaddedChar   => CharArrayBuilder.asInstanceOf[Builder[Any, A]]
    case PackedFloat  | PaddedFloat  => FloatArrayBuilder.asInstanceOf[Builder[Any, A]]
    case PackedDouble | PaddedDouble => DoubleArrayBuilder.asInstanceOf[Builder[Any, A]]
    case PackedBoolean               => BitArrayBuilder.asInstanceOf[Builder[Any, A]]
    case typeA: ValType[A]           => ValArrayBuilder[A](typeA)
  }
}

private[container] class RefArrayBuilders {
  implicit def RefArrayBuilder[A]: RefArray.Builder[A] = new RefArray.Builder[A]
}

private[container] class ValArrayBuilders extends RefArrayBuilders {
  implicit def ValArrayBuilder[A](implicit typeA: ValType[A]): ValArray.Builder[A] = new ValArray.Builder[A]
}

private[container] class AllArrayBuilders extends ValArrayBuilders {
  implicit def ByteArrayBuilder: ByteArray.Builder = new ByteArray.Builder
  implicit def ShortArrayBuilder: ShortArray.Builder = new ShortArray.Builder
  implicit def IntArrayBuilder: IntArray.Builder = new IntArray.Builder
  implicit def LongArrayBuilder: LongArray.Builder = new LongArray.Builder
  implicit def CharArrayBuilder: CharArray.Builder = new CharArray.Builder
  implicit def FloatArrayBuilder: FloatArray.Builder = new FloatArray.Builder
  implicit def DoubleArrayBuilder: DoubleArray.Builder = new DoubleArray.Builder
  implicit def BitArrayBuilder: BitArray.Builder = new BitArray.Builder
}
