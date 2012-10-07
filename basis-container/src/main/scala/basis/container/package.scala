/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

import basis.data._

package object container extends PrimitiveBuffers {
  def ArrayBuffer[A](implicit typeA: MemType[A]): Buffer[Array[A], A] { type State = Array[A] } = {
    import ValType._
    (typeA match {
      case typeA: RefType[A]           => RefBuffer[A]
      case PackedByte                  => ByteBuffer
      case PackedShort  | PaddedShort  => ShortBuffer
      case PackedInt    | PaddedInt    => IntBuffer
      case PackedLong   | PaddedLong   => LongBuffer
      case PackedFloat  | PaddedFloat  => FloatBuffer
      case PackedDouble | PaddedDouble => DoubleBuffer
      case PackedBoolean               => BitBuffer
      case typeA: ValType[A]           => ValBuffer[A](typeA)
    }).asInstanceOf[Buffer[Array[A], A] { type State = Array[A] }]
  }
}

package container {
  private[container] class DefaultBuffers {
    implicit def DefaultBuffer[A]: Buffer[Any, A] { type State = List[A] } =
      ListBuffer[A].asInstanceOf[Buffer[Any, A] { type State = List[A] }]
  }
  
  private[container] class RefBuffers extends DefaultBuffers {
    implicit def ListBuffer[A]: ListBuffer[A] = new ListBuffer[A]
    
    implicit def RefBuffer[A]: RefBuffer[A] = new RefBuffer[A]
  }
  
  private[container] class ValBuffers extends RefBuffers {
    implicit def ValBuffer[A](implicit typeA: ValType[A]): ValBuffer[A] = new ValBuffer[A]
  }
  
  private[container] class PrimitiveBuffers extends ValBuffers {
    implicit def ByteBuffer: ByteBuffer = new ByteBuffer
    implicit def ShortBuffer: ShortBuffer = new ShortBuffer
    implicit def IntBuffer: IntBuffer = new IntBuffer
    implicit def LongBuffer: LongBuffer = new LongBuffer
    implicit def FloatBuffer: FloatBuffer = new FloatBuffer
    implicit def DoubleBuffer: DoubleBuffer = new DoubleBuffer
    implicit def BitBuffer: BitBuffer = new BitBuffer
  }
}
