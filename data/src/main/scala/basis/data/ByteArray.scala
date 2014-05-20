//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._

trait ByteArray extends Any with Loader with Storer {
  def ++ (that: Loader): ByteArray with ByteOrder[Endian]

  def toArray: Array[Byte]
}

object ByteArray extends ByteOrder[NativeEndian] with Allocator[ByteArray with ByteOrder[NativeEndian]] {
  override def endian: NativeEndian = NativeEndian

  override val empty: ByteArray with ByteOrder[NativeEndian] = {
    if (endian.isBig) ByteArrayBE.empty
    else if (endian.isLittle) ByteArrayLE.empty
    else throw new MatchError(endian)
  }.asInstanceOf[ByteArray with ByteOrder[NativeEndian]]

  override def apply(data: Array[Byte]): ByteArray with ByteOrder[NativeEndian] = {
    if (endian.isBig) ByteArrayBE(data)
    else if (endian.isLittle) ByteArrayLE(data)
    else throw new MatchError(endian)
  }.asInstanceOf[ByteArray with ByteOrder[NativeEndian]]

  override def apply(size: Long): ByteArray with ByteOrder[NativeEndian] = {
    if (endian.isBig) ByteArrayBE(size)
    else if (endian.isLittle) ByteArrayLE(size)
    else throw new MatchError(endian)
  }.asInstanceOf[ByteArray with ByteOrder[NativeEndian]]

  implicit override def Framer: Framer with ByteOrder[NativeEndian] with State[ByteArray with ByteOrder[NativeEndian]] = {
    if (endian.isBig) ByteArrayBE.Framer
    else if (endian.isLittle) ByteArrayLE.Framer
    else throw new MatchError(endian)
  }.asInstanceOf[Framer with ByteOrder[NativeEndian] with State[ByteArray with ByteOrder[NativeEndian]]]

  override def toString: String = "ByteArray"
}
