//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.collections._

trait ByteBuffer extends Any with Loader with Storer {
  def ++ (that: Loader): ByteBuffer with ByteOrder[Endian]

  def toArray: Array[Byte]
}

object ByteBuffer extends ByteOrder[NativeEndian] with ByteFactory[ByteBuffer with ByteOrder[NativeEndian]] {
  override def endian: NativeEndian = NativeEndian

  override val empty: ByteBuffer with ByteOrder[NativeEndian] = {
    if (endian.isBig) ByteBufferBE.empty: ByteBuffer
    else if (endian.isLittle) ByteBufferLE.empty: ByteBuffer
    else throw new MatchError(endian)
  }.asInstanceOf[ByteBuffer with ByteOrder[NativeEndian]]

  implicit override def Framer(): Framer with ByteOrder[NativeEndian] with State[ByteBuffer with ByteOrder[NativeEndian]] = {
    if (endian.isBig) ByteBufferBE.Framer()
    else if (endian.isLittle) ByteBufferLE.Framer()
    else throw new MatchError(endian)
  }.asInstanceOf[Framer with ByteOrder[NativeEndian] with State[ByteBuffer with ByteOrder[NativeEndian]]]

  override def toString: String = "ByteBuffer"
}
