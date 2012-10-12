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
      case typeA: RefType[A]           => new RefArrayBuffer[A]
      case PackedByte                  => new ByteArrayBuffer
      case PackedShort  | PaddedShort  => new ShortArrayBuffer
      case PackedInt    | PaddedInt    => new IntArrayBuffer
      case PackedLong   | PaddedLong   => new LongArrayBuffer
      case PackedFloat  | PaddedFloat  => new FloatArrayBuffer
      case PackedDouble | PaddedDouble => new DoubleArrayBuffer
      case PackedBoolean               => new BitArrayBuffer
      case typeA: ValType[A]           => new ValArrayBuffer[A]()(typeA)
    }).asInstanceOf[Buffer[Any, A] { type State = Array[A] }]
  }
}

package container {
  private[container] class DefaultBuffers {
    implicit def DefaultBuffer[A]: Buffer[Any, A] { type State = Seq[A] } =
      new ListBuffer[A].asInstanceOf[Buffer[Any, A] { type State = Seq[A] }]
  }
  
  private[container] class RefBuffers extends DefaultBuffers {
    implicit def ListBuffer[A]: ListBuffer[A] = new ListBuffer[A]
    
    implicit def RefArrayBuffer[A]: RefArrayBuffer[A] = new RefArrayBuffer[A]
  }
  
  private[container] class ValBuffers extends RefBuffers {
    implicit def ValArrayBuffer[A : ValType]: ValArrayBuffer[A] = new ValArrayBuffer[A]
  }
  
  private[container] class PrimitiveBuffers extends ValBuffers {
    implicit def ByteArrayBuffer: ByteArrayBuffer = new ByteArrayBuffer
    implicit def ShortArrayBuffer: ShortArrayBuffer = new ShortArrayBuffer
    implicit def IntArrayBuffer: IntArrayBuffer = new IntArrayBuffer
    implicit def LongArrayBuffer: LongArrayBuffer = new LongArrayBuffer
    implicit def FloatArrayBuffer: FloatArrayBuffer = new FloatArrayBuffer
    implicit def DoubleArrayBuffer: DoubleArrayBuffer = new DoubleArrayBuffer
    implicit def BitArrayBuffer: BitArrayBuffer = new BitArrayBuffer
  }
}
