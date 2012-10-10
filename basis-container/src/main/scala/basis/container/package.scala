/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

import basis.data._

package object container extends PrimitiveBuffers {
  def ArrayBuffer[A](implicit typeA: MemType[A]): Buffer[Any, A] { type State = Array[A] } = {
    import ValType._
    (typeA match {
      case typeA: RefType[A]           => new RefArrayBuffer[Any, A]
      case PackedByte                  => new ByteArrayBuffer[Any]
      case PackedShort  | PaddedShort  => new ShortArrayBuffer[Any]
      case PackedInt    | PaddedInt    => new IntArrayBuffer[Any]
      case PackedLong   | PaddedLong   => new LongArrayBuffer[Any]
      case PackedFloat  | PaddedFloat  => new FloatArrayBuffer[Any]
      case PackedDouble | PaddedDouble => new DoubleArrayBuffer[Any]
      case PackedBoolean               => new BitArrayBuffer[Any]
      case typeA: ValType[A]           => new ValArrayBuffer[Any, A]()(typeA)
    }).asInstanceOf[Buffer[Any, A] { type State = Array[A] }]
  }
}

package container {
  private[container] class DefaultBuffers {
    implicit def DefaultBuffer[A]: Buffer[Any, A] { type State = Seq[A] } =
      new ListBuffer[Any, A].asInstanceOf[Buffer[Any, A] { type State = Seq[A] }]
  }
  
  private[container] class RefBuffers extends DefaultBuffers {
    implicit def ListBuffer[A]: ListBuffer[List[_], A] = new ListBuffer
    
    implicit def RefArrayBuffer[A]: RefArrayBuffer[Array[_], A] =
      new RefArrayBuffer[Array[_], A]
  }
  
  private[container] class ValBuffers extends RefBuffers {
    implicit def ValArrayBuffer[A : ValType]: ValArrayBuffer[Array[_], A] =
      new ValArrayBuffer[Array[_], A]
  }
  
  private[container] class PrimitiveBuffers extends ValBuffers {
    implicit def ByteArrayBuffer: ByteArrayBuffer[Any] = new ByteArrayBuffer[Any]
    implicit def ShortArrayBuffer: ShortArrayBuffer[Any] = new ShortArrayBuffer[Any]
    implicit def IntArrayBuffer: IntArrayBuffer[Any] = new IntArrayBuffer[Any]
    implicit def LongArrayBuffer: LongArrayBuffer[Any] = new LongArrayBuffer[Any]
    implicit def FloatArrayBuffer: FloatArrayBuffer[Any] = new FloatArrayBuffer[Any]
    implicit def DoubleArrayBuffer: DoubleArrayBuffer[Any] = new DoubleArrayBuffer[Any]
    implicit def BitArrayBuffer: BitArrayBuffer[Any] = new BitArrayBuffer[Any]
  }
}
