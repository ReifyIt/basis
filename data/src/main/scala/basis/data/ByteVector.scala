//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._

abstract class ByteVector extends Loader {
  def mutateByte(address: Long, value: Byte): ByteVector with ByteOrder[Endian]

  def mutateShort(address: Long, value: Short): ByteVector with ByteOrder[Endian]

  def mutateInt(address: Long, value: Int): ByteVector with ByteOrder[Endian]

  def mutateLong(address: Long, value: Long): ByteVector with ByteOrder[Endian]

  def mutateFloat(address: Long, value: Float): ByteVector with ByteOrder[Endian]

  def mutateDouble(address: Long, value: Double): ByteVector with ByteOrder[Endian]

  def ++ (that: Loader): ByteVector with ByteOrder[Endian]
}

object ByteVector extends ByteOrder[NativeEndian] with ByteFactory[ByteVector with ByteOrder[NativeEndian]] {
  override def endian: NativeEndian = NativeEndian

  override val empty: ByteVector with ByteOrder[NativeEndian] = {
    if (endian.isBig) ByteVectorBE.empty
    else if (endian.isLittle) ByteVectorLE.empty
    else throw new MatchError(endian)
  }.asInstanceOf[ByteVector with ByteOrder[NativeEndian]]

  implicit override def Framer: Framer with ByteOrder[NativeEndian] with State[ByteVector with ByteOrder[NativeEndian]] = {
    if (endian.isBig) ByteVectorBE.Framer
    else if (endian.isLittle) ByteVectorLE.Framer
    else throw new MatchError(endian)
  }.asInstanceOf[Framer with ByteOrder[NativeEndian] with State[ByteVector with ByteOrder[NativeEndian]]]

  override def toString: String = "ByteVector"
}
